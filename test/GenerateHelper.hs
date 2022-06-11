{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module GenerateHelper where

import Control.Monad (guard)
import Data.Data (Data)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Tuple qualified as Tuple
import Generics.SYB qualified as SYB
import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Datatype (applySubstitution)
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import System.FilePath qualified as FilePath

-- thingy :: Q TH.Type
-- thingy = do
--   ty@(TH.ForallT (arrTyVar : rest) x y) <- TH.reifyType 'Generic.fromList
--   let tyRemoved = TH.ForallT rest x y
--   let [name] = SYB.listify (const @_ @TH.Name True) arrTyVar
--   primitiveArrayTy <- [t|Primitive.Array|]
--   let ty' = applySubstitution (Map.fromList [(name, primitiveArrayTy)]) ty
--   let tyRemoved' = applySubstitution (Map.fromList [(name, primitiveArrayTy)]) tyRemoved
--   TH.runIO $ do
--     putStrLn "before "
--     putStrLn $ TH.pprint ty'
--     putStrLn "after"
--     putStrLn $ TH.pprint tyRemoved'
--     putStrLn "tyRemoved clean"
--     putStrLn $ TH.pprint $ SYB.everywhere (SYB.mkT $ TH.mkName . TH.nameBase) tyRemoved'
--   pure ty'

modPrelude :: Text
modPrelude = "-- Generated file, do not edit!\n"

extensions :: Text
extensions = "{-# LANGUAGE StarIsType #-}"

textE :: Text -> Q TH.Exp
textE = TH.stringE . T.unpack

writeModuleContents :: Text -> Text -> IO ()
writeModuleContents modName contents = do
  cwd <- Directory.makeAbsolute "./"
  let fullPath = cwd </> "src/" </> modNameToPath modName
  let dir = FilePath.takeDirectory fullPath
  Directory.createDirectoryIfMissing True dir
  TIO.writeFile fullPath contents

modNameToPath :: Text -> FilePath
modNameToPath modName = do
  let namePath = T.replace "." "/" modName
  T.unpack namePath <.> ".hs"

generateInstantiatedModule :: Text -> [([TH.Name], Q TH.Type)] -> Q Text
generateInstantiatedModule modName nameTys = do
  nameTys <- traverse sequenceA nameTys
  nameTys <- pure [(name, ty) | (names, ty) <- nameTys, name <- names]
  generateInstantiatedModule' modName nameTys

generateInstantiatedModule' :: Text -> [(TH.Name, TH.Type)] -> Q Text
generateInstantiatedModule' modName nameTys = do
  let names = fmap fst nameTys
      modHeader =
        TL.toStrict $
          TLB.toLazyText $
            "module "
              <> TLB.fromText modName
              <> "(\n"
              <> TLB.fromText (T.intercalate ",\n" $ fmap (T.pack . TH.nameBase) names)
              <> "\n)\nwhere"
  (imports, contents) <- fold <$> traverse (uncurry generateInstantiated . Tuple.swap) nameTys
  pure $
    T.intercalate
      "\n"
      [ modPrelude,
        extensions,
        modHeader,
        fromString imports,
        fromString contents
      ]

generateInstantiated :: TH.Type -> TH.Name -> Q (String, String)
generateInstantiated ty name = do
  decs <- generateInstantiatedDec ty name
  let imports = List.intercalate "\n" $ fmap generateImports decs
  pure ("", pprint' decs)

generateImports :: TH.Dec -> String
generateImports dec =
  neededImports dec
    & fmap (\modName -> "import " ++ modName ++ " qualified")
    & List.intercalate "\n"
    & (++ "\n")

generateInstantiatedDec :: TH.Type -> TH.Name -> Q [TH.Dec]
generateInstantiatedDec ty name = do
  [d|
    $(TH.varP $ TH.mkName $ TH.nameBase name) :: $(makeInstantiatedType ty name) =
      $(TH.varE $ TH.mkName $ "Data.HashMap.Mutable.Generic." ++ TH.nameBase name)
    |]

makeInstantiatedType :: TH.Type -> TH.Name -> Q TH.Type
makeInstantiatedType arrTy name = do
  ty@(TH.ForallT ((TH.nameBase . head . listifyNames -> "arr") : _) _ _) <- TH.reifyType name
  Just ty' <- pure $ applyType ty [arrTy]
  pure ty'

applyType :: TH.Type -> [TH.Type] -> Maybe TH.Type
applyType ty tys = do
  TH.ForallT tyVarBinders ctxt ty <- pure ty
  let tysLen = length tys
  guard $ tysLen <= length tyVarBinders
  let (SYB.listify (const @_ @TH.Name True) -> names, tyVarBinders') = splitAt tysLen tyVarBinders
      subst = Map.fromList $ zip names tys
  pure $ applySubstitution subst $ TH.ForallT tyVarBinders' ctxt ty

pprint' :: (TH.Ppr a, Data a) => a -> String
pprint' = TH.pprint . normalizeNames

normalizeNames :: Data a => a -> a
normalizeNames = normalizeNamesWith normalizeName

normalizeName :: TH.Name -> TH.Name
-- normalizeName name = TH.mkName $ fromMaybe "" (fmap (++ ".") $ TH.nameModule name) ++ TH.nameBase name

normalizeName name = TH.mkName $ TH.nameBase name

normalizeNamesWith :: Data a => (TH.Name -> TH.Name) -> a -> a
normalizeNamesWith f = SYB.everywhere (SYB.mkT f)

listifyNames :: SYB.GenericQ [TH.Name]
listifyNames = SYB.listify (const @_ @TH.Name True)

neededImports :: Data a => a -> [String]
neededImports x =
  listifyNames x
    & fmap (\name -> TH.nameModule name)
    & catMaybes

-- patchModName :: String -> String
-- patchModName =
-- modPatches :: HashMap String String
-- modPatches = fromList [("Data.Hashable.Class", "Data.Hashable")]
