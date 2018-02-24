-- | Expect to success the parses
module IdTags.ParserTest where

import Test.Tasty (TestTree)

-- |
-- `
-- data Foo
--      ^^^
-- `
-- and
-- `
-- data Foo a
--      ^^^^^
-- `
-- and
-- `
-- data (<!>) a b
--      ^^^^^^^^^
-- `
test_parser_parses_data_types :: [TestTree]
test_parser_parses_data_types = undefined


-- |
-- `
-- data Foo = Bar
--            ^^^
-- `
-- and
-- `
-- data Foo = Bar Int
--            ^^^^^^^
-- `
-- and
-- `
-- data Foo = (<!>) Bar Baz
--            ^^^^^^^^^^^^^
-- `
test_parser_parses_data_value_constructors :: [TestTree]
test_parser_parses_data_value_constructors = undefined


-- |
-- `
-- data Foo : Bar -> Type where
--      ^^^^^^^^^^^^^^^^^
-- `
-- and
-- `
-- data (<!>) : Foo -> Bar -> Type where
--      ^^^^^^^^^^^^^^^^^^^^^^^^^^
-- `
test_parser_parses_gadts :: [TestTree]
test_parser_parses_gadts = undefined


-- |
-- `
-- data Foo : Bar -> Type where
--   MkFoo : Baz -> Foo
--   ^^^^^^^^^^^^^^^^^^
-- `
-- and
-- `
-- data (<!>) : Foo -> Bar -> Type where
--   Baz : Int <!> Int
--   ^^^
-- `
test_parser_parses_gadt_value_constructors :: [TestTree]
test_parser_parses_gadt_value_constructors = undefined


-- |
-- `
-- record Foo where
--        ^^^
-- `
test_parser_parses_record_structures :: [TestTree]
test_parser_parses_record_structures = undefined


-- |
-- `
-- record Foo where
--   constructor MkFoo
--               ^^^^^
-- `
test_parser_parses_record_constructors :: [TestTree]
test_parser_parses_record_constructors = undefined

-- |
-- `
-- record Foo where
--   constructor MkFoo
--   num : Int
--   ^^^^^^^^^
-- `
test_parser_parses_record_fields :: [TestTree]
test_parser_parses_record_fields = undefined


-- |
-- `
-- interface Foo a where
--           ^^^^^
-- `
-- and
-- `
-- interface Bar a => Foo a where
--           ^^^^^^^^^^^^^^
-- `
-- and
-- `
-- interface (<!>) a b where
--           ^^^^^^^^^
-- `
test_parser_parses_interfaces :: [TestTree]
test_parser_parses_interfaces = undefined


-- |
-- `
-- Foo Baz where
-- ^^^^^^^
-- `
-- and
-- `
-- (<!>) Int Int where
-- ^^^^^^^^^^^^^
-- `
-- and
-- `
-- implementation Foo Baz where
--                ^^^^^^^
-- `
-- and
-- `
-- instance Foo Baz where
--          ^^^^^^^
-- `
test_parser_parses_instances :: [TestTree]
test_parser_parses_instances = undefined


-- |
-- `
-- foo : Bar -> Baz
-- ^^^^^^^^^^^^^^^^
-- `
-- and
-- `
-- foo : (x : Bar) -> Baz
-- ^^^^^^^^^^^^^^^^^^^^^^
-- `
-- and
-- `
-- (<!>) : Bar -> Baz -> Power
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^
-- `
test_parser_parses_functions :: [TestTree]
test_parser_parses_functions = undefined


-- |
-- `
-- Foo = Bar
-- ^^^^^^^^^
-- `
test_parser_parses_type_aliases :: [TestTree]
test_parser_parses_type_aliases = undefined


-- |
-- `
-- f : Foo -> Bar
-- f x = ?some
-- where
--   foo : Baz -> Power
--   ^^^^^^^^^^^^^^^^^^
-- `
test_parser_parses_functions_under_a_function :: [TestTree]
test_parser_parses_functions_under_a_function = undefined


-- |
-- `
-- namespace Foo
--           ^^^
-- `
test_parser_parses_namespaces :: [TestTree]
test_parser_parses_namespaces = undefined
