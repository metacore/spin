INTERFACE NetText;
IMPORT Salnet;

PROCEDURE FmtIp(ip:Salnet.IpAddr): TEXT ;
PROCEDURE TextToIp(ipText: TEXT): Salnet.IpAddr ;
PROCEDURE FmtEther(ether:Salnet.EtherAddr): TEXT ;
PROCEDURE TextToEther(etherText: TEXT; (*OUT*) VAR ether: Salnet.EtherAddr) ;

END NetText.
