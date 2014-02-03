{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Protocol.NetSNMP as SNMP


snmp_community :: SNMP.Community
snmp_community = "public"

ext_oid :: SNMP.RawOID
ext_oid = [1, 3, 6, 1, 4, 1, 2021, 8, 1]


main = do
  SNMP.initialize
  r <- SNMP.snmpWalk SNMP.snmp_version_1 "dogbert.hq.c3d2.de" snmp_community ext_oid
  print r
  
