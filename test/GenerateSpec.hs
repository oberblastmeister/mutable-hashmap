{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module GenerateSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import NeatInterpolation (trimming)
import System.Directory (makeAbsolute)
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath
import Test.Hspec

spec :: Spec
spec = do
  xit "generate files" $ do
    mapM_
      (uncurry writeModule)
      [ ("Data.HashMap.Mutable.Boxed", "Primitive.Array"),
        ("Data.HashMap.Mutable.Boxed.Small", "Primitive.SmallArray"),
        ("Data.HashMap.Mutable.Prim", "Primitive.PrimArray"),
        ("Data.HashMap.Mutable.Unlifted", "Contiguous.UnliftedArray")
      ]

writeModule :: Text -> Text -> IO ()
writeModule name arrayTy = do
  let contents = makeModuleContents name arrayTy
  cwd <- makeAbsolute "./"
  let namePath = T.replace "." "/" name
  let fullPath = cwd </> "src/" </> T.unpack namePath <.> ".hs"
  let dir = FilePath.takeDirectory fullPath
  Directory.createDirectoryIfMissing True dir
  TIO.writeFile fullPath contents

makeModuleContents :: Text -> Text -> Text
makeModuleContents name arrayTy =
  [trimming|
    -- Generated file, do not edit!

    {-# OPTIONS_GHC -Wno-redundant-constraints #-}
    {-# OPTIONS_GHC -Wno-unused-imports #-}

    module $name
      ( HashMap,
        new,
        newWithCapacity,
        delete,
        lookup,
        toList,
        fromList,
      )
    where

    import Prelude hiding (lookup)
    import Data.Hashable (Hashable)
    import Control.Monad.Primitive (PrimMonad, PrimState)
    import Data.HashMap.Mutable.Generic qualified as Generic
    import Data.HashMap.Mutable.Generic (BothElement)
    import Data.Primitive qualified as Primitive
    import Data.Primitive.Contiguous qualified as Contiguous

    type HashMap = Generic.HashMap $arrayTy

    new :: (BothElement $arrayTy k v, PrimMonad m) => m (HashMap (PrimState m) k v)
    new = Generic.new
    {-# INLINE new #-}

    newWithCapacity :: (BothElement $arrayTy k v, PrimMonad m) => Int -> m (HashMap (PrimState m) k v)
    newWithCapacity = Generic.newWithCapacity
    {-# INLINE newWithCapacity #-}

    delete :: (BothElement $arrayTy k v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m ()
    delete = Generic.delete
    {-# INLINE delete #-}

    lookup :: (BothElement $arrayTy k v, PrimMonad m, Hashable k) => k -> HashMap (PrimState m) k v -> m (Maybe v)
    lookup = Generic.lookup
    {-# INLINE lookup #-}

    toList :: (BothElement $arrayTy k v, PrimMonad m) => HashMap (PrimState m) k v -> m [(k, v)]
    toList = Generic.toList
    {-# INLINE toList #-}

    fromList :: (BothElement $arrayTy k v, PrimMonad m, Hashable k) => [(k, v)] -> m (HashMap (PrimState m) k v)
    fromList = Generic.fromList
    {-# INLINE fromList #-}
  |]
