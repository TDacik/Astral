# Astral

<img align="left" width="200" src="https://excel.fit.vutbr.cz/submissions/2022/038/38_nahled.png">

Astral is a solver for separation logic based on translation to SMT. Astral supports clasical separation logic as well as [strong-separation logic](https://arxiv.org/abs/2001.06235) (SSL), in particular, it supports several features that are not supported by other solvers such as limited support for magic wands and arbitrary mixing and nesting of boolean and spatial connectives (see [supported fragment](#supported-fragment)). 

The solver is currently able to use two SMT backends &ndash; [Z3](https://github.com/Z3Prover/z3) and [cvc5](https://github.com/cvc5/cvc5).

</br>
</br>

## Supported fragment
Let $x, y$ be variables. Atomic formulae are defined as follow:

| Atomic formula     | Input syntax        | Semantics |
|---------------------|---------------------|-----------|
| $x = y$             | (= x y)             | $s(x) = s(y)$ and $h = \emptyset$ |
| $x \neq y$          | (distinct x y)      | $s(x) \neq s(y)$ and $h = \emptyset$ |
| $x \mapsto y$       | (pto x y)           | $h = \lbrace s(x) \mapsto s(y) \rbrace$ |
| $\mathsf{emp}$      | emp                 | $h = \emptyset$ |
| $\mathsf{ls}$(x,y)  | (ls x y)            | $\mathsf{dom}(h) = \emptyset \land s(x) = s(y) \text{ or there exist } n \geq 1, \ell_0, ..., \ell_n \text{ such that } \mathsf{distinct}(\ell_0, ..., \ell_n)$ $\text{ and } h = \lbrace \ell_0 \mapsto \ell_1, ..., \ell_{n-1} \mapsto \ell_n \rbrace \text{ and } s(x) = \ell_0, s(y) = \ell_n$|

Let $\phi, \psi$ be formulae.

| Connective        | Input syntax              | Semantics |
|-----------------------------|---------------------------|-----------|
| $\varphi \land \psi$                | (and $\varphi$ $\psi$)       | $(s,h) \models \varphi \text{ and } (s,h) \models \psi$       |
| $\varphi \lor \psi$                 | (or $\varphi$ $\psi$)        | $(s,h) \models \varphi \text{ or }  (s,h) \models \psi$       |
| $\varphi \land_\neg \psi$           | (and $\varphi \text{ (not } \psi$)) | $(s,h) \models \varphi \text{ and } (s,h) \not\models \psi$       |
| $\varphi \ast \psi$                 | (sep $\phi$ $\psi$)         | $\exists h_1, h_2. h = h_1 \uplus^s h_2 \text{ and } (s,h_1) \models \varphi \text{ and } (s,h_2) \models \psi$       |
| $\varphi -\kern-.46em{\circledast}$  $\psi$   | (septraction $\phi$ $\psi$) | $\exists h_1. (s,h_1) \models \varphi \text { and } h_1 \uplus^s h_2 \neq \bot \text{ and } (s,h_1 \uplus^s h_2) \models \psi$       |

### Example input
A formula $x \mapsto y -\kern-.46em{\circledast}$ $\mathsf{ls}(x,y)$ can be represented as:
```smt
(declare-sort Loc 0)      ;; Declaration of location sort. Currently fixed to this form by Astral.
(declare-heap (Loc Loc))  ;; Declaration of heap sort. Currently fixed to this form by Astral.

;; Declaration of location variables
(declare-const x Loc)
(declare-const y Loc)

;; Input formula
(assert
  (septraction
    (pto x y)
    (ls x y)
  )
)

(check-sat)
```


## Installation & Usage
After cloning the repository, run `opam install .` to install Astral. Then the solver can be run by the command `astral formula.smt2`.

## Related Papers
* Dacík, Tomáš. [A Decision Procedure for Strong-Separation Logic](https://www.fit.vut.cz/study/thesis/25151/ ). Brno, 2022. Master's Thesis. Brno University of Technology, Faculty of Information Technology. 2022-08-25. Supervised by Vojnar Tomáš. 

## Contact
If you have any questions, do not hesitate to contact the tool/method authors:
* [**Tomáš Dacík**](https://www.fit.vut.cz/person/idacik/.en) <[idacik@fit.vut.cz](mailto:idacik@fit.vut.cz)>
* [**Adam Rogalewicz**](https://www.fit.vut.cz/person/rogalew/.en) <[rogalew@fit.vut.cz](mailto:rogalew@fit.vut.cz)>
* [**Tomáš Vojnar**](https://www.fit.vut.cz/person/vojnar/.en) <[vojnar@fit.vut.cz](mailto:vojnar@fit.vut.cz)>
* [**Florian Zuleger**](https://informatics.tuwien.ac.at/people/florian-zuleger)<[florian.zuleger@tuwien.ac.at](mailto:florian.zuleger@tuwien.ac.at)>

## License
The tool is available under MIT license.

## Acknowledgements
The development was supported by projects [SNAPPY](https://www.fit.vut.cz/research/project/1342/.en) and [AIDE](https://www.fit.vut.cz/research/project/1615/.en) of the Czech Science Foundation. The main author of the tool was supported by Brno Ph.D. Talent Scholarship funded by the Brno City Municipality.
