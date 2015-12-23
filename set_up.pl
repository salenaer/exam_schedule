setup_small:-consult(data/small).
setup_largeShort:-consult(data/largeShort).
setup_largeLong:-consult(data/largeLong).

setup_files:-consult(is_valid),
			 consult(cost),
			 consult(violates_sc),
			 consult(find_optimal),
			 consult(pritty_print).