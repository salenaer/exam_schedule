setup_small:-consult(data/small),
			 setup_files.
setup_largeShort:-consult(data/largeShort),
				  setup_files.
setup_largeLong:-consult(data/largeLong),
				 setup_files.

setup_files:-use_module(is_valid),
			 use_module(pritty_print),
			 use_module(violates_sc),
			 use_module(cost),
			 use_module(find_optimal).