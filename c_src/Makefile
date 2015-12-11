all: $(CURDIR)/../priv/pseudo_node $(CURDIR)/../priv/epmd_proxy

$(CURDIR)/../priv/pseudo_node $(CURDIR)/../priv/epmd_proxy:
	gcc -o $@ $<

$(CURDIR)/../priv/epmd_proxy: epmd_port_proxy.c
$(CURDIR)/../priv/pseudo_node: epmd_client.c

clean:
	rm -f $(CURDIR)/../priv/pseudo_node $(CURDIR)/../priv/epmd_proxy
