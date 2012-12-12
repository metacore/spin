#
# Copyright 1996, University of Washington
# All rights reserved.
# See COPYRIGHT file for a full description
#
# HISTORY
# 28-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
#	added m3tcp dependencies
#
# 06-Oct-96  Robert Grimm (rgrimm) at the University of Washington
#       added stats to the CLIENTSPINDLES
#

BROKENPLEXUSSPINDLES = atm t3 an1
BROKENCLIENTSPINDLES = atmClient t3Client an1Client

PLEXUSSPINDLES = ether ip icmp udp tcp m3tcp

CLIENTSPINDLES = dns etherClient ipClient icmpClient udpClient	\
		 tcpClient loopback osfClient stats vmtp	\
		 ip6 ip6Client icmp6 icmp6Client ether6Client dht

# we use the wildcard so only the subtrees that exist here are built
PROGRAMS= $(wildcard plexus $(PLEXUSSPINDLES) $(CLIENTSPINDLES))

install: $(PROGRAMS)
$(PROGRAMS):
	$(MAKE) -C $@ install

clean clean_all clobber clobber_all::
	@for i in $(PROGRAMS); do \
		echo -n "$$i:	" ;\
		$(MAKE) -C $$i $@; \
		done


# this line allows dir names to be used as Makefile targets
.PHONY: $(PROGRAMS)

m3tcp: ipClient dns
ipClient: etherClient
ip6Client: ether6Client
etherClient: ip
etherClient: ip6
ip6: ether
ip6Client: ip6 icmp6
dht: ip6 ip6Client
