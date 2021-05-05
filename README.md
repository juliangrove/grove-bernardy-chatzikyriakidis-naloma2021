# grove-bernardy-chatzikyriakidis-naloma2021

Code for Grove, Bernardy, and Chatzikyriakidis 2021. To reproduce the examples
from the paper, clone this repository and do:

	cd grove-bernardy-chatzikyriakidis-naloma2021
	nix-shell --run "cabal v2-run rsa"
	
You will be prompted to say which example from the paper you'd like to run (1 or
2), the temperature (α) at which you want to run the model, and the values of
log-cost you'd like to use for noun phrases and, then, pronouns. To get the
value of the first row in Table 1, for example, you should enter 1, 0.5, 0, 0.
