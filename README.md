# grove-bernardy-chatzikyriakidis-naloma2021

Code for Grove, Bernardy, and Chatzikyriakidis 2021. You should have
[Nix](https://nixos.org/) installed. To reproduce the examples from the paper,
do:

	git clone --recurse-submodules https://github.com/juliangrove/grove-bernardy-chatzikyriakidis-naloma2021 
	cd grove-bernardy-chatzikyriakidis-naloma2021
	nix-shell --run "cabal v2-run rsa"
	
You will be prompted to say which example from the paper you'd like to run (1 or
2), the temperature (α) at which you want to run the model, and the values of
log-cost you'd like to use for noun phrases and, then, pronouns. To get the
value of the first row in Table 1, for example, you should enter 1, 0.5, 0, 0.

The output is displayed as a list of pairs whose first component is a list of
anaphora resolution choices for each pronoun appearing in the evaluated sentence
(displayed right-to-left) and whose second component is a probability. For
example,

	result: [([0,0],0.2),([0,1],0.3),([1,0],0.4),([1,1],0.1)]

when computed for a sentence with two pronouns and two possible antecedents,
would mean that both pronouns are resolved to the linearly closest antecedent
(0) with probability 0.2; that the linearly first pronoun is resolved to the
second-closest antecedent (1) and the linearly second pronoun is resolved to the
closest antecedent (0) with probability 0.3; that the linearly first pronoun is
resolved to the closest antecedent (0) and the linearly second pronoun is
resolved to the second-closest antecedent (1) with probability 0.4; etc.
