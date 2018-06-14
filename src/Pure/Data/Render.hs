{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, MultiWayIf, FlexibleInstances, ViewPatterns #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
module Pure.Data.Render where

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from pure-dom
import Pure.DOM

-- from pure-json
import Pure.Data.JSON

-- from pure-lifted
import Pure.Data.Lifted

-- from pure-txt
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Pure.Data.Txt as Txt

-- from containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- from base
import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.IORef
import qualified Data.List as List
import Data.Monoid
import Data.Proxy
import Data.Typeable
import System.IO.Unsafe
import Unsafe.Coerce

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#endif

newtype StaticHTML = StaticHTML { htmlText :: Txt } deriving (Eq,Ord)
instance ToTxt StaticHTML where
  toTxt (StaticHTML htmlt) = htmlt
instance FromTxt StaticHTML where
  fromTxt = StaticHTML
instance Monoid StaticHTML where
  mempty = fromTxt mempty
  mappend htmlt1 htmlt2 = fromTxt $ toTxt htmlt1 <> toTxt htmlt2
#ifdef USE_TEMPLATE_HASKELL
instance Lift StaticHTML where
  lift (StaticHTML (Txt.unpack -> htmls)) = [| StaticHTML (Txt.pack htmls) |]
#endif

staticHTML :: View -> StaticHTML
staticHTML = fromTxt . toTxt

shtml :: Txt -> Features -> StaticHTML -> View
shtml tag features = RawView Nothing tag features . toTxt

selfClosing tag = tag `elem` selfclosing
  where
    selfclosing =
      ["area","base","br","col","frame","command"
      ,"embed","hr","img","input","keygen","link"
      ,"meta","param","source","track","wbr"
      ]

instance ToJSON Features where
  toJSON f =
#ifdef __GHCJS__
    objectValue $
#endif
      object
        [ "classes"    .= toJSON (Set.toList $ classes f)
        , "styles"     .= toJSON (Map.toList $ styles f)
        , "attributes" .= toJSON (Map.toList $ attributes f)
        , "properties" .= toJSON (Map.toList $ properties f)
        ]

instance FromJSON Features where
  parseJSON o0 = do
    flip (withObject "Features") o0 $ \o -> do
      classes    <- Set.fromList <$> o .: "classes"
      styles     <- Map.fromList <$> o .: "styles"
      attributes <- Map.fromList <$> o .: "attributes"
      properties <- Map.fromList <$> o .: "properties"
      let listeners = []; lifecycles = []
      return Features_ {..}

renderer :: (View,MVar View) -> View
renderer = ComponentIO $ \self ->
  def
    { construct = do
        (v,_) <- ask self
        return v
    , mounted = do
        v <- look self
        (_,mv) <- ask self
        putMVar mv v
        modify_ self $ \_ _ -> NullView Nothing
    , render = \_ -> id
    }


-- ToJSON for View currently doesn't handle Portals.
-- To do so, we would need the option to encode the
-- destination as a path selector....
instance ToJSON View where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
      go cv@ComponentView { record = r, ..} =
        case r of
          Nothing -> do
            go $ unsafePerformIO $ do
              e <- create "div"
              mv <- newEmptyMVar
              inject e (renderer (cv,mv))
              takeMVar mv

          Just ref ->
            go (unsafePerformIO (readIORef (crView ref)))

      go (SomeView _ v) = go (view v)

      go (TextView _ c) =
        object
          [ "type" .= ("TextView" :: Txt)
          , "content" .= c
          ]

      go (RawView _ t fs c) =
        object
          [ "type" .= ("RawView" :: Txt)
          , "tag" .= t
          , "features" .= toJSON fs
          , "content" .= c
          ]

      go (KHTMLView _ t fs ks) =
        object
          [ "type" .= ("KHTMLView" :: Txt)
          , "tag" .= t
          , "features" .= toJSON fs
          , "keyedChildren" .= toJSON ks
          ]

      go (HTMLView _ t fs cs) =
        object
          [ "type" .= ("HTMLView" :: Txt)
          , "tag" .= t
          , "features" .= toJSON fs
          , "children" .= toJSON cs
          ]

      go (KSVGView _ t fs xs ks) =
        object
          [ "type" .= ("KSVGView" :: Txt)
          , "tag" .= t
          , "features" .= toJSON fs
          , "xlinks" .= toJSON (Map.toList xs)
          , "keyedChildren" .= toJSON ks
          ]

      go (SVGView _ t fs xs cs) =
        object
          [ "type" .= ("SVGView" :: Txt)
          , "tag" .= t
          , "features" .= toJSON fs
          , "xlinks" .= toJSON (Map.toList xs)
          , "children" .= toJSON cs
          ]

      go (NullView _) =
        object [ "type" .= ("NullView" :: Txt) ]

      go (LazyView f a) = go (view (f a))

      go _ = object []

instance FromJSON View where
  parseJSON o0 = do
    flip (withObject "View") o0 $ \o -> do
      t <- o .: "type"
      case t :: Txt of
        "TextView" -> do
          c <- o .: "content"
          pure $ TextView Nothing c
        "RawView" -> do
          t <- o .: "tag"
          fs <- o .: "features"
          c <- o .: "content"
          pure $ RawView Nothing t fs c
        "KHTMLView" -> do
          t <- o .: "tag"
          fs <- o .: "features"
          ks <- o .: "keyedChildren"
          pure $ KHTMLView Nothing t fs ks
        "HTMLView" -> do
          t <- o .: "tag"
          fs <- o .: "features"
          cs <- o .: "children"
          pure $ HTMLView Nothing t fs cs
        "KSVGView" -> do
          t <- o .: "tag"
          fs <- o .: "features"
          xs <- o .: "xlinks"
          ks <- o .: "keyedChildren"
          pure $ KSVGView Nothing t fs (Map.fromList xs) ks
        "SVGView" -> do
          t <- o .: "tag"
          fs <- o .: "features"
          xs <- o .: "xlinks"
          cs <- o .: "children"
          pure $ SVGView Nothing t fs (Map.fromList xs) cs
        _ -> pure $ NullView Nothing

instance Show Features where
  show = fromTxt . toTxt

instance ToTxt Features where
  toTxt Features_ {..} =
    let
        cs    = "class=\""
              <> (mconcat
                  $ List.intersperse " "
                  $ Set.toList
                  $ Set.delete "" classes
                 )
              <> "\""

        ss    = "style=\""
              <> (mconcat
                  $ List.intersperse ";"
                  $ fmap (\(k,v) -> k <> ":" <> v)
                  $ Map.toList styles
                 )
              <> "\""

        attrs = mconcat
              $ List.intersperse " "
              $ fmap (\(k,v) -> k <> "=\"" <> v <> "\"")
              $ Map.toList attributes

        props = mconcat
              $ List.intersperse " "
              $ fmap (\(k,v) -> k <> "=\"" <> v <> "\"")
              $ Map.toList properties
    in
        cs <> " " <> ss <> " " <> attrs <> " " <> props

nullFeatures Features_ {..} =
  styles == mempty
    && classes == mempty
    && attributes == mempty
    && properties == mempty

instance Show View where
  show = go 0
    where
      go n TextView {..} = List.replicate n ' ' <> fromTxt content <> "\n"
      go n RawView {..} =
        "<" <> fromTxt tag
            <> (if nullFeatures features then "" else " " <> show features)
            <>
        ">" <> fromTxt content <>
        "</" <> fromTxt tag <> ">"

      go n KHTMLView {..} =
        go n HTMLView { children = fmap snd keyedChildren, ..}

      go n KSVGView {..} =
        let fs = features { attributes = Map.union (attributes features) xlinks }
        in go n HTMLView { features = fs, children = fmap snd keyedChildren, .. }

      go n SVGView {..} =
        let fs = features { attributes = Map.union (attributes features) xlinks }
        in go n HTMLView { features = fs, .. }

      go n HTMLView {..} =
        List.replicate (2 * n) ' ' <>
        "<" <> fromTxt tag <> (if nullFeatures features then "" else " " <> show features)
            <> if | tag == "?xml"     -> "?>\n"
                  | tag == "!doctype" -> ">\n"
                  | selfClosing tag   -> "/>\n"
                  | otherwise         ->
                      ">\n" <> List.concatMap (go (n + 1)) children <>
                       List.replicate (2 * n) ' ' <> "</" <> fromTxt tag <> ">\n"

      go n cv@ComponentView { record = r, ..} =
        case r of
          Nothing ->
            show $ unsafePerformIO $ do
              e <- create "div"
              mv <- newEmptyMVar
              inject e (renderer (cv,mv))
              takeMVar mv

          Just ref -> show $ unsafePerformIO (readIORef (crView ref))

      go n (SomeView _ c) = show (view c)

      go n (LazyView f a) = go n (view (f a))

      go _ _ = ""


instance ToTxt View where
  toTxt TextView {..} = content

  toTxt RawView {..} =
    "<" <> tag <> (if nullFeatures features then "" else " " <> toTxt features) <>
      ">" <> content <> "</" <> tag <> ">"

  toTxt KHTMLView {..} =
    toTxt HTMLView { children = fmap snd keyedChildren, ..}

  toTxt HTMLView {..} =
    if tag == "?xml" then
      "<?xml"  <> (if nullFeatures features then "" else " " <> toTxt features) <> "?>"
    else
      "<" <> tag <> (if nullFeatures features then "" else " " <> toTxt features) <>
        if selfClosing tag then
          "/>"
        else
          ">" <> mconcat (map toTxt children) <> "</" <> tag <> ">"

  toTxt SVGView {..} =
    let fs = features { attributes = Map.union (attributes features) xlinks }
    in toTxt HTMLView { features = fs, ..}

  toTxt KSVGView {..} =
    let fs = features { attributes = Map.union (attributes features) xlinks }
    in toTxt HTMLView { features = fs, children = List.map snd keyedChildren, .. }

  toTxt cv@ComponentView { record = r, ..} =
    case r of
      Nothing -> toTxt $ unsafePerformIO $ do
        e <- create "div"
        mv <- newEmptyMVar
        inject e (renderer (cv,mv))
        takeMVar mv

      Just ref -> toTxt $ unsafePerformIO (readIORef (crView ref))

  toTxt (SomeView _ c) = toTxt (view c)

  toTxt (LazyView f a) = toTxt (view (f a))

  toTxt _ = mempty

instance ToTxt [View] where
  toTxt = mconcat . fmap toTxt

