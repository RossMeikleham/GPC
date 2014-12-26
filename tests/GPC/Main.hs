module Main where

import GPC.ParserTests
import GPC.TypeTests
import GPC.CodeGenTests
import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [parserTests, typeTests, codeGenTests]
