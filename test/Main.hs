{-# LANGUAGE
    OverloadedStrings 
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Ldap.Parsers
import Data.ByteString (ByteString)
import qualified Ldap.Client as L
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "LDAP filter parser test" $
      decodeFilter ldapFilterString @?= Right ldapFilter
  , testCase "LDAP filter encoding test" $
      encodeFilter ldapFilter @?= ldapFilterString
  ]

ldapFilterString :: ByteString
ldapFilterString = "(&(objectCategory=Person)(sAMAccountName=*)(|(memberOf=cn=fire,ou=users,dc=company,dc=com)(memberOf=cn=wind,ou=users,dc=company,dc=com)(memberOf=cn=water,ou=users,dc=company,dc=com)(memberOf=cn=heart,ou=users,dc=company,dc=com)))"

ldapFilter :: L.Filter
ldapFilter = L.And $ NE.fromList
  [ L.Attr "objectCategory" L.:= "Person"
  , L.Present $ L.Attr "sAMAccountName"
  , L.Or $ NE.fromList
      [ L.Attr "memberOf" L.:= "cn=fire,ou=users,dc=company,dc=com"
      , L.Attr "memberOf" L.:= "cn=wind,ou=users,dc=company,dc=com"
      , L.Attr "memberOf" L.:= "cn=water,ou=users,dc=company,dc=com"
      , L.Attr "memberOf" L.:= "cn=heart,ou=users,dc=company,dc=com"
      ]
  ]

deriving instance Eq L.Filter
deriving instance Show L.Filter
