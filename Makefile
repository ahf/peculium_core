PROJECT = peculium_core

DEPS = lager ucrypto edown eleveldb gproc ranch nat_upnp triq

dep_lager = https://github.com/basho/lager.git 2.0.0
dep_ucrypto = https://github.com/ahf/erlang-ucrypto.git master
dep_edown = https://github.com/esl/edown.git master
dep_eleveldb = https://github.com/basho/eleveldb.git master
dep_gproc = https://github.com/esl/gproc.git master
dep_ranch = https://github.com/extend/ranch.git master
dep_nat_upnp = https://github.com/benoitc/nat_upnp.git master
dep_triq = https://github.com/krestenkrab/triq.git master

ERLC_OPTS = +debug_info +'{parse_transform, lager_transform}'

include erlang.mk
