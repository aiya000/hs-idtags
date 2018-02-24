import Test.DocTest (doctest)

main :: IO ()
main = doctestIt "src" "" []

-- | Test files of `path` and `dependencies` on `baseDir` by doctest
doctestIt :: FilePath -> FilePath -> [FilePath] -> IO ()
doctestIt baseDir path dependencies = do
  let core_target = "-i" ++ baseDir
  doctest $ (core_target:dependencies) ++ [path]
