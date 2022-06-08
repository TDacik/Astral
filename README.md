# Astral
Astral is a prototype implementation of a decision procedure for a fragment of [strong-separation logic](https://arxiv.org/abs/2001.06235) (SSL) based on a translation to SMT. 

## Strong-Separation Logic

Let $x, y$ be variables. Atomic formulae are defined as follow:

| Atomic formulae | Input syntax | Semantics |
|----------|--------------|-----------|
| $x = y$             | (= x y)       | $s(x) = s(y)$ and $h = \emptyset$ |
| $x \neq y$          | (distinct x y)| $s(x) \neq s(y)$ and $h = \emptyset$ |
| $x \mapsto y$       | (pto x y)     | $h = \{s(x) \mapsto s(y)\}$ |
| $\mathsf{emp}$      | emp sep.emp   | $h = \emptyset$ |
| $\mathsf{ls}$(x,y)  | (ls x y)      | iff dom$(h) = \emptyset \text{ and } s(x) = s(y) \text{ or there exist } n \geq 1, \ell_0, ..., \elll_n \text{ such that} \mathsf{distinct}(\ell_0, ..., \ell_n) \text{ and } h = \{\ell_0 \mapsto \ell_1, ..., \ell_{n-1} \mapsto \ell_n\} \text{ and } s(x) = \ell_0, s(y) = \ell_n$|

Let $\phi, \psi$ be formulae.

| Boolean connectives         | Input syntax              | Semantics |
|-----------------------------|---------------------------|-----------|
| $\varphi \land \psi$           | (and $\varphi$ $\psi$)       | ...       |
| $\varphi \lor \psi$            | (or $\varphi$ $\psi$)        | ...       |
| $\varphi \land_\neg \psi$      | (and $\varphi$ (not $\psi$)) | ...       |

| Spatial connectives                           | Input syntax                | Semantics |
|-----------------------------------------------|-----------------------------|-----------|
| $\varphi \ast \psi$                           | (sep $\phi$ $\psi$)         | ...       |
| $\varphi -\kern-.46em{\circledast}$  $\psi$   | (septraction $\phi$ $\psi$) | ...       |

### Example input
A formula $x \mapsto y -\kern-.46em{\circledast}$ $\mathsf{ls}(x,y)$:
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
```


## Usage

## Contact
If you have any questions, do not hesitate to contact the tool/method authors:
* **Tomáš Dacík** <[xdacik00@stud.fit.vutbr.cz](mailto:xdacik00@stud.fit.vutbr.cz)>
* [**Tomáš Vojnar**](https://www.fit.vut.cz/person/vojnar/) <[vojnar@fit.vutbr.cz](mailto:vojnar@fit.vutbr.cz)>

## License
The plugin is available under MIT license.
