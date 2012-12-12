INTERFACE DhtMapping;
IMPORT DhtIP4Tbl, DhtIP6Tbl;
VAR
  ip4node4TOip6node4 : DhtIP4Tbl.Default;
  ip4node6TOip6node6 : DhtIP4Tbl.Default;
  ip6node4TOip4node4 : DhtIP6Tbl.Default;
  ip6node6TOip4node6 : DhtIP6Tbl.Default;
END DhtMapping.
