;;; (**) Graph isomorphism
;;;
;;; Two graphs (n1 e1) and (n2 e2) are isomorphic if there is a
;;; bijection f: n1 -> n2 such that for any nodes x,y of n1, x and y
;;; are adjacent if and only if f(x) and f(y) are adjacent.
;;;
;;; Write a function that determines whether two graphs are
;;; isomorphic. Hint: Use an open-ended list.
(in-package :99-problems)

(defun isomorph-p (g1 g2)
  "Return t if ``g1'' and ``g2'' are isomorphic graphs."
  ;; This is the VF2 Algorithm, as described in the following papers:
  ;;
  ;; [1] Luigi P. Cordella, Pasquale Foggia, Carlo Sansone, Mario Vento,
  ;; “A (Sub)Graph Isomorphism Algorithm for Matching Large Graphs”,
  ;; IEEE Transactions on Pattern Analysis and Machine Intelligence,
  ;; vol. 26, no. 10, pp. 1367-1372, Oct. 2004.
  ;; http://ieeexplore.ieee.org/iel5/34/29305/01323804.pdf
  ;;
  ;; [2] L. P. Cordella, P. Foggia, C. Sansone, M. Vento,
  ;; “An Improved Algorithm for Matching Large Graphs”,
  ;; 3rd IAPR-TC15 Workshop on Graph-based Representations in Pattern Recognition,
  ;; Cuen, pp. 149-159, 2001.
  ;; http://amalfi.dis.unina.it/graph/db/papers/vf-algorithm.pdf
  ;;
  ;; This implementation is also loosely based on the Python
  ;; implementation of the VF2 algorithm found in the NetworkX
  ;; library, specifically the file isomorphvf2.py.
  ;; https://github.com/networkx/networkx/blob/master/networkx/algorithms/isomorphism/isomorphvf2.py
  ;;
  ;; Although this function is far too large (~140 lines!), the vast
  ;; majority of the lines are in small, locally-declared functions of
  ;; 1-5 lines each. These could easily be broken out into separate
  ;; functions, but I've kept them local to avoid the need to pass the
  ;; state around explicitly as a parameter. If you ignore all the
  ;; functions defined in the LABELS form, the leftover is only 22
  ;; lines long, and most of that is related to state initialization.

  (let (;; The algorithm presented in [1] and [2] works on directed
	;; graphs. It's easy to modify the algorithm to work with
	;; undirected graphs, mainly by combining the in_x and out_x
	;; vectors into a single vector. See the difference between
	;; the GraphMatcher and DiGraphMatcher classes in the NetworkX
	;; python implementation for an example. We take the approach
	;; here of just converting the input graphs to directed graphs
	;; and stick to the algorithm presented in the paper.
	(g1 (convert-to 'directed g1))
	(g2 (convert-to 'directed g2))
	;; See the comment below for a description of graph-state.
	(graph-state (make-hash-table))
	;; Depth tracks the current depth in the SSR tree. It's
	;; incremented each time a new pair of nodes is added to the
	;; partial solution. Used to restore the state of the 'in and
	;; 'out terminal sets when backtracking.
	(depth 0)
	;; A stack of node-pairs that have been added to the partial
	;; solution. The last pair added is at (first node-stack). Used
	;; when restoring the 'core mappings when backtracking.
	(node-stack '()))

    ;; Initialize graph-state. Graph-state is a hash-table that holds
    ;; various peices of state that are needed by the VF2
    ;; algorithm. Each input graph has it's own set of associated
    ;; state. In the source papers ([1] and [2]), these pieces of
    ;; state are stored in separate variables, not in a
    ;; hash-table. For example, the variables core_1 and core_2 from
    ;; the paper are vectors that store the current mapping of
    ;; vertices in G1 to vertices in G2, whereas in the current
    ;; implementation, the mappings are stored in hash-tables and are
    ;; accessed as
    ;;
    ;;     (gethash 'core (gethash g1 graph-state)) and
    ;;     (gethash 'core (gethash g2 graph-state))
    ;;
    ;; respectively. A convenience function, get-state is provided so
    ;; that accessing core_1 can be shortened to
    ;;
    ;;     (get-state 'core g1)
    ;;
    ;; Likewise for the variables core_2, in_1, in_2, out_1, and out_2
    ;; described in the paper.
    ;;
    ;; In other words, all the per-graph state from the paper is
    ;; represented here in a two-tier hash-table, where the first tier
    ;; is indexed by the input graph, and the second tier is indexed
    ;; by a symbol indicating which peice of state is stored under
    ;; that key (e.g. 'in, 'out, 'core, 'num-vertices, etc.). The
    ;; advantage of this approach is that certain parts of the
    ;; algorithm can be abstracted to operate on either set of state,
    ;; and only the graph needs to be passed as an arg rather than all
    ;; the state variables separately. The downside is that every
    ;; state access requires two extra hash lookups.
    ;;
    ;; The state variables are described in the paper in the section
    ;; titled "Data Structures and Implementation Issues". Here is a
    ;; summary of the data stored in graph-state. Note that each of
    ;; the input graphs have their own set of associated state.
    ;;
    ;; Key                   Description
    ;; -------------------------------------------------------------------------
    ;; 'core                 Core is a hash-table that holds the current partial
    ;;                       solution. That is, each 'core is a mapping between
    ;;                       vertices in M_1(s) and M_2(s). Specifically,
    ;;                       forall X in (hash-table-keys (get-state 'core g1))
    ;;
    ;;                       (gethash (gethash X (get-state 'core g1))
    ;;                                (get-state 'core g2))
    ;;                       ==> X
    ;;
    ;;                       And vice-versa.
    ;;
    ;; 'in                   In is a hash-table where the keys represent the
    ;;                       set of all vertices that are either in M_x(s) or
    ;;                       else are a predecessor to a vertex in M_x(s).
    ;;                       The values represent the depth in SSR tree at which
    ;;                       associated vertex entered the corresponding set.
    ;;
    ;; 'out                  Out is the same as 'in, but with successors instead
    ;;                       of predecessors.
    ;;
    ;; The six above mappings are the only graph-dependent state
    ;; mentioned in the papers. The following keys are also included
    ;; in graph-state in this implementation, but are not specifically
    ;; required by the algorithm presented in the papers.
    ;;
    ;; Key                   Description
    ;; -------------------------------------------------------------------------
    ;; 'num-vertices         The number of vertices in the graph. This value is
    ;;                       used first as a quick check to rule out obvious
    ;;                       non-isomorphs. If the number of vertices differs
    ;;                       between the two graphs, they cannot be isomorphic.
    ;;                       This value is also used to verify whether an
    ;;                       isomorph has been found, i.e. when the number of
    ;;                       vertices in 'core is equal to num-vertices.
    ;;
    ;; 'predecessors         A hash-table where the keys are the vertices of the
    ;;                       graph and the values are a list of that vertex's
    ;;                       predecessors.
    ;;
    ;; 'successors           Like 'predecessor, but for successors.
    ;;
    ;; 'adj-maxtrix          A two-dimensional array that represents the
    ;;                       adjacency matrix for the graph. Useful for
    ;;                       determining the number of edges between vertices.
    ;;
    ;; 'vertices->indices    A hash-table where the keys are the vertices of the
    ;;                       graph and the values are the index of that vertex
    ;;                       in the adjacency matrix. Allows looking up an entry
    ;;                       in the adjacency matrix when one only knows the
    ;;                       vertex.
    (loop for g in (list g1 g2)
       do (let ((g-state (make-hash-table)))
	    (loop for key in '(in out core predecessors successors)
	       do (setf (gethash key g-state) (make-hash-table)))
            (let ((predecessors (gethash 'predecessors g-state))
		  (successors (gethash 'successors g-state)))
	      (loop for v in (vertices g)
		 do (setf (gethash v predecessors) (predecessors g v))
		 do (setf (gethash v successors) (successors g v))))
	    (setf (gethash 'num-vertices g-state) (order g))
	    (multiple-value-bind (adj-matrix vertices->indices)
		(adjacency-matrix g)
	      (setf (gethash 'adj-matrix g-state) adj-matrix)
	      (setf (gethash 'vertices->indices g-state) vertices->indices))
	    (setf (gethash g graph-state) g-state)))
    (labels ((get-state (key graph)
	       (gethash-or-die key (gethash-or-die graph graph-state)))
             (predecessors. (vertex graph)
	       (gethash-or-die vertex (get-state 'predecessors graph)))
	     (successors. (vertex graph)
	       (gethash-or-die vertex (get-state 'successors graph)))
	     (core-vertices (graph)
	       (alexandria:hash-table-keys (get-state 'core graph)))
	     (non-core-vertices (graph)
	       (set-difference (vertices graph)
			       (core-vertices graph)))
	     (core-vertex-p (vertex graph)
	       (hash-set-p vertex (get-state 'core graph)))
	     (core-get (vertex graph)
	       (gethash vertex (get-state 'core graph)))
	     (terminal-set (graph in/out)
	       (loop for v being the hash-keys in (get-state in/out graph)
		  unless (core-vertex-p v graph)
		  collect v))
	     (vertex->index (v graph)
	       (gethash-or-die v (get-state 'vertices->indices graph)))
	     (num-edges-between (v1 v2 graph)
	       (aref (get-state 'adj-matrix graph)
		     (vertex->index v1 graph)
		     (vertex->index v2 graph)))
	     (r-loops (n m)
	       ;; This rule is not mentioned in the paper and is not
	       ;; strictly required. Without this check, the algorithm
	       ;; will fail on r-pred in the next recursive invocation
	       ;; of match. This idea is stolen from the NetworkX
	       ;; python implementation.
	       (= (num-edges-between n n g1) (num-edges-between m m g2)))
	     (pred/succ-check (pred/succ-fn n n-graph m m-graph)
	       (every (lambda (n-prime)
			(some (lambda (m-prime)
				(and (eql m-prime (core-get n-prime n-graph))
				     (= (num-edges-between n n-prime n-graph)
					(num-edges-between m m-prime m-graph))))
			      (funcall pred/succ-fn m m-graph)))
		      (intersection (funcall pred/succ-fn n n-graph)
				    (core-vertices n-graph))))
	     (r-pred (n m)
	       ;; Rpred(s,n,m) <==>
	       ;; (∀n′ ∈ M₁(s) ∩ Pred(G₁,n) ∃m′ ∈ Pred(G₂,m) | (n′,m′) ∈ M(s)) ∧
	       ;; (∀m′ ∈ M₂(s) ∩ Pred(G₂,m) ∃n′ ∈ Pred(G₁,n) | (n′,m′) ∈ M(s)) ∧
	       (and (pred/succ-check #'predecessors. n g1 m g2)
		    (pred/succ-check #'predecessors. m g2 n g1)))
	     (r-succ (n m)
	       ;; Rsucc(s,n,m) <==>
	       ;; (∀n′ ∈ M₁(s) ∩ Succ(G₁,n) ∃m′ ∈ Succ(G₂,m) | (n′,m′) ∈ M(s)) ∧
	       ;; (∀m′ ∈ M₂(s) ∩ Succ(G₂,m) ∃n′ ∈ Succ(G₁,n) | (n′,m′) ∈ M(s)) ∧
	       (and (pred/succ-check #'successors. n g1 m g2)
		    (pred/succ-check #'successors. m g2 n g1)))
	     (in/out/new-check (n n-restrictor m m-restrictor)
	       ;; For graph-subgraph isomorphism, switch the = to >=
	       (and (= (length (intersection (successors. n g1) n-restrictor))
		       (length (intersection (successors. m g2) m-restrictor)))
		    (= (length (intersection (predecessors. n g1) n-restrictor))
		       (length (intersection (predecessors. m g2) m-restrictor)))))
	     (r-in (n m)
	       ;; Rin(s,n,m) <==>
	       ;; (Card(Succ(G₁,n) ∩ T₁^in(s)) = Card(Succ(G₂,m) ∩ T₂^in(s))) ∧
	       ;; (Card(Pred(G₁,n) ∩ T₁^in(s)) = Card(Pred(G₂,m) ∩ T₂^in(s))) ∧
	       (in/out/new-check n (terminal-set g1 'in)
				 m (terminal-set g2 'in)))
	     (r-out (n m)
	       ;; Rout(s,n,m) <==>
	       ;; (Card(Succ(G₁,n) ∩ T₁^out(s)) = Card(Succ(G₂,m) ∩ T₂^out(s))) ∧
	       ;; (Card(Pred(G₁,n) ∩ T₁^out(s)) = Card(Pred(G₂,m) ∩ T₂^out(s))) ∧
	       (in/out/new-check n (terminal-set g1 'out)
				 m (terminal-set g2 'out)))
	     (big-enya (graph)
	       ;; Spelled Ñ(s) in the paper.
	       (reduce #'set-difference
		       (list (vertices graph)
			     (core-vertices graph)
			     (terminal-set graph 'in)
			     (terminal-set graph 'out))))
	     (r-new (n m)
	       ;; Rnew(s,n,m) <==>
	       ;; (Card(Ñ₁(s) ∩ Pred(G₁,n)) = Card(Ñ₂(s) ∩ Pred(G₂,m))) ∧
	       ;; (Card(Ñ₁(s) ∩ Succ(G₁,n)) = Card(Ñ₂(s) ∩ Succ(G₂,m))) ∧
	       (in/out/new-check n (big-enya g1)
				 m (big-enya g2)))
	     (feasible (n m)
	       (and (r-loops n m) (r-pred n m) (r-succ n m)
		    (r-in n m) (r-out n m) (r-new n m)))
	     (min. (lst)
	       (if (null lst)
		   nil
		   ;; Sorting via sxhash will not produce lexicographic ordering.
		   (list (reduce (lambda (x y) (if (< (sxhash x) (sxhash y)) x y))
				 lst))))
	     (candidate-pairs ()
	       ;; From [1]
	       ;; 2.1 Computation of the Candidate Pairs Set P(s)
	       ;;
	       ;; The set P(s) of all the possible pairs candidate to
	       ;; be added to the current state is obtained by
	       ;; considering first the sets of the nodes directly
	       ;; connected to G₁(s) and G₂(s). Let us denote with
	       ;; T₁^out(s) and T₂^out(s) the sets of nodes, not yet
	       ;; in the partial mapping, that are the destination of
	       ;; branches starting from G₁(s) and G₂(s),
	       ;; respectively; similarly, with T₁^in(s) and T₂^in(s),
	       ;; we will denote the sets of nodes, not yet in the
	       ;; partial mapping, that are the origin of branches
	       ;; ending into G₁(s) and G₂(s).  The set P(s) will be
	       ;; made of all the node pairs (n,m), with n belonging
	       ;; to T₁^out(s) and m to T₂^out(s), unless one of these
	       ;; two sets is empty. In this case, the set P(s) is
	       ;; likewise obtained by considering T₁^in(s) and
	       ;; T₂^in(s), respectively. In presence of not connected
	       ;; graphs, for some state s, all of the above sets may
	       ;; be empty. In this case, the set of candidate pairs
	       ;; making up P(s) will be the set P^d(s) of all the
	       ;; pairs of nodes not contained neither in G₁(s) nor in
	       ;; G₂(s).
	       ;;
	       ;; From [2]
	       ;; 2.2 Definition of the set P(s) and of the
	       ;;     feasibility function F(s,n,m)
	       ;;
	       ;; ...The set P(s) is constructed as follows: if both
	       ;; T₁^out(s) and T₂^out(s) are not empty, then
	       ;;
	       ;;     P(s) = T₁^out(s) × {min T₂^out(s)}
	       ;;
	       ;; where the min refers to the node in T₂^out(s) which
	       ;; has the smallest label (actually, any other total
	       ;; ordering criterion could be used). If instead both
	       ;; T₁^out(s) and T₂^out(s) are empty, and both T₁^in(s)
	       ;; and T₂^in (s) are not, then
	       ;;
	       ;;     P(s) = T₁in(s) × {min T₂in(s)}
	       ;;
	       ;; Finally, if all the four terminal sets are empty,
	       ;;
	       ;;     P(s) = (N₁ − M₁(s)) × {min (N₂ − M₂(s))}
	       (or (cartesian-product (terminal-set g1 'out)
				      (min. (terminal-set g2 'out)))
		   (cartesian-product (terminal-set g1 'in)
				      (min. (terminal-set g2 'in)))
		   (cartesian-product (non-core-vertices g1)
				      (min. (non-core-vertices g2)))))
	     (update-state (n m)
	       (incf depth)
	       (push (cons n m) node-stack)
	       (setf (gethash n (get-state 'core g1)) m)
	       (setf (gethash m (get-state 'core g2)) n)

	       ;; Only add the nodes to 'in and 'out if they are not
	       ;; already present.
	       (alexandria:ensure-gethash n (get-state 'in g1) depth)
	       (alexandria:ensure-gethash m (get-state 'in g2) depth)
	       (alexandria:ensure-gethash n (get-state 'out g1) depth)
	       (alexandria:ensure-gethash m (get-state 'out g2) depth)

               (dolist (p (predecessors. n g1))
		 (alexandria:ensure-gethash p (get-state 'in g1) depth))
	       (dolist (p (predecessors. m g2))
		 (alexandria:ensure-gethash p (get-state 'in g2) depth))
	       (dolist (p (successors. n g1))
		 (alexandria:ensure-gethash p (get-state 'out g1) depth))
	       (dolist (p (successors. m g2))
		 (alexandria:ensure-gethash p (get-state 'out g2) depth)))
	     (restore-state ()
	       (destructuring-bind (n . m) (pop node-stack)
		 (remhash n (get-state 'core g1))
		 (remhash m (get-state 'core g2)))
	       (dolist (hash-table (list (get-state 'in g1) (get-state 'in g2)
					 (get-state 'out g1) (get-state 'out g2)))
		 (maphash (lambda (k v) (when (= v depth) (remhash k hash-table)))
			  hash-table))
	       (decf depth))
	     (found-isomorph-p ()
	       (= (hash-table-count (get-state 'core g1))
		  (get-state 'num-vertices g2)))
	     (match ()
	       (or (found-isomorph-p)
		   (loop for (n m) in (candidate-pairs)
		      thereis (and (feasible n m)
				   ;; A rare prog2 sighting! Isn't she beautiful?
				   (prog2 (update-state n m)
				          (match)
				     (restore-state)))))))
      ;; Rule out trivial non-ismorphs. If the number of vertices and
      ;; sorted-degrees don't match, then the two graphs cannot be
      ;; isomorphs. Graph-subgraph isomorphism is still possible, but
      ;; beside the point.
      (when (and (= (get-state 'num-vertices g1) (get-state 'num-vertices g2))
		 (equal (sorted-degrees g1) (sorted-degrees g2)))
	(match)))))


;;; General-Purpose Utilities

(defmacro let-returning (binding-forms &body body)
  "Bind forms with let, execute body, then return the bound variables."
  `(let ,binding-forms
     ,@body
     (values ,@(loop for b in binding-forms collect (if (atom b) b (car b))))))

(defun gethash-or-die (key hash-table)
  "Return (gethash key hash-table) if the value exists; otherwise,
throw an error."
  (multiple-value-bind (value found) (gethash key hash-table)
    (if found value (error "~a not found in ~a~%" key hash-table))))

(defun hash-set-p (key hash-table)
  "Return t if ``key'' is set in ``hash-table''; otherwise, return nil."
  (nth-value 1 (gethash key hash-table)))

(defun zip (&rest lists)
  "Return a list of n-tuples made by applying #'list to successive
corresponding elements from the n input ``lists''. The length of the
resulting list will be equal to the length of the shortest input
list."
  (apply #'mapcar #'list lists))

(defun nth-triangular (n)
  "Return the nth triangular number."
  (/ (* n (+ n 1)) 2))

(defun flat-list (&rest args)
  "Return a flattened list of ``args''."
  (flatten args))

(defun permutations (lst &key (length (length lst)))
  "Return a list of all permutations of ``lst'' of length ``length''."
  (let-returning (permutations)
    (alexandria:map-permutations (lambda (x) (push x permutations))
				 lst :length length)))

(defun combinations (lst &key (length (length lst)))
  "Return a list of all combinations of ``lst'' of length ``length''."
  (let-returning (combinations)
    (alexandria:map-combinations (lambda (x) (push x combinations))
				 lst :length length)))

(defun translate (table tree)
  "Return a new tree where for each (A . B) in the alist ``table'', A
has been substituted for B in ``tree''."
  (labels ((recur (tree)
	     (cond ((null tree) nil)
		   ((assoc tree table :test #'equal)
		    (cdr (assoc tree table :test #'equal)))
		   ((symbolp tree) tree)
		   (t (cons (recur (car tree)) (recur (cdr tree)))))))
    (recur tree)))


;;; Graph-Related Functions and Methods

(defun order (graph)
  "Return the order of ``GRAPH''."
  (length (vertices graph)))

(defgeneric adjacency-matrix (graph)
  (:documentation "Return an adjacency matrix for graph."))

(defmethod adjacency-matrix ((graph directed-graph))
  (loop
     with adj-list = (adjacency graph)
     with adj-matrix = (make-array (list (order graph) (order graph)))
     with vertices->indices = (make-hash-table)
     for v1 in (vertices graph)
     for i upfrom 0
     do (setf (gethash v1 vertices->indices) i)
     do (loop
	   for v2 in (vertices graph)
	   for j upfrom 0
	   do (setf (aref adj-matrix i j) (count v2 (adjacent-nodes adj-list v1))))
     finally (return (values adj-matrix vertices->indices))))

(defgeneric degree (graph vertex)
  (:documentation "Return the degree of ``vertex'' in ``graph''."))

(defmethod degree ((graph directed-graph) vertex)
  (loop for (v neighbors) in (adjacency graph)
     ;; Self-loops count double. Once as in-degree, and once as out-degree.
     if (eq v vertex) sum (loop for n in neighbors if (eq v n) sum 2 else sum 1)
     else sum (count vertex neighbors)))

(defmethod degree ((graph undirected-graph) vertex)
  (length (second (assoc vertex (adjacency graph)))))

(defun degrees (graph)
  "Return a list of the degree of each vertex in ``graph''."
  (mapcar (lambda (v) (degree graph v)) (vertices graph)))

(defun sorted-degrees (graph &key (predicate #'>))
  "Return a sorted list of the degree of each vertex in ``graph''."
  (sort (degrees graph) predicate))

(defgeneric predecessors (graph vertex)
  (:documentation "Return a list of all predecessors of ``VERTEX'' in ``GRAPH''"))

(defmethod predecessors ((graph directed-graph) vertex)
  (loop for (v successors) in (adjacency graph)
       if (member vertex successors) collect v))

(defgeneric successors (graph vertex)
  (:documentation "Return a list of all successors of ``VERTEX'' in ``GRAPH''"))

(defmethod successors ((graph directed-graph) vertex)
  (second (assoc vertex (adjacency graph))))


;;; Test Helpers

(defun set-set-equal (a b)
  "Return true if ``a'' and ``b'' are equal when considered as sets of
sets."
  (set-equal a b :test #'set-equal))

(defun set-of-graphs-equal (a b)
  "Return true if ``a'' and ``b'' are equal when considered as sets of
  graphs."
  (set-equal a b :test #'graph-equal))

(defun set-of-isomorphs-p (a b)
  "Return true if ``a'' and ``b'' are set-equal under #'isomorph-p."
  (set-equal a b :test #'isomorph-p))

(flet ((generate-isomorphs-from-permuted-vertices (graph list-of-permuted-vertices)
	 "Return a list of isomorphs of ``graph'' using the given
``list-of-permuted-vertices'' to rename the edges of ``graph''."
	 ;; Use the permuted vertices to rename the edges of ``graph''.
	 ;; For example, suppose we have the following graph:
	 ;;
	 ;;    (mk-graph '(a b c d) '((a b) (a c) (b c) (c d)))
	 ;;
	 ;; which can be represented as
	 ;;
	 ;;        A ---- C
	 ;;        |    / |
	 ;;        |  /   |
	 ;;        B      D
	 ;;
	 ;; And suppose we have '(c a b d) as the current value of our
	 ;; permuted vertices. Then our isomorph will be
	 ;;
	 ;;    (mk-graph '(a b c d) '((c a) (c b) (a b) (b d)))
	 ;;
	 ;;        C ---- B                   D  C
	 ;;        |    / |        or         | / \
	 ;;        |  /   |   equivalently    |/   \
	 ;;        A      D                   B-----A
	 (loop for permuted-vertices in list-of-permuted-vertices
	    collect (make-instance
		     (class-of graph)
		     :data (list (vertices graph)
				 (translate (pairlis (vertices graph)
						     permuted-vertices)
					    (edges graph)))))))
  (defun generate-random-isomorphs (graph num-isomorphs)
    "Return a list of ``num-isomorphs'' random isomorphs of ``graph''."
    ;; Because we blindly collect random permutations of the vertices
    ;; of ``graph'', we run the risk of "too many" duplicates if
    ;; ``num-isomorphs'' is too large a fraction of the total number
    ;; of possible permutations.
    (assert (< num-isomorphs
	       (/ (alexandria:count-permutations (length (vertices graph))) 2)))
    (generate-isomorphs-from-permuted-vertices
     graph
     (loop repeat num-isomorphs
	collect (alexandria:shuffle (copy-list (vertices graph))))))

  (defun generate-all-isomorphs (graph)
    "Return a list of all isomorphs of ``graph''."
    ;; The output of this function grows as n!, where n is the number
    ;; of vertices in ``graph''.
    ;;
    ;; Prevent generating an insane number of graphs.
    (assert (< (length (vertices graph)) 8))
    (generate-isomorphs-from-permuted-vertices
     graph (permutations (vertices graph)))))

(defun generate-loops (vertices)
  "Return a list of all self-loop edges for ``vertices''."
  (zip vertices vertices))

(defun generate-all-edges
    (vertices &key allow-loops (graph-type 'undirected-graph))
  "Return a list of all edges possible among ``vertices'' for a graph
of type ``graph-type''. Self-loops are included or excluded based on
the value of ``allow-loops''."
  ;; NB: Loops are usually forbidden in undirected graphs.
  ;;
  ;; Let n = (length vertices).
  ;; if eq graph-type 'undirected-graph
  ;;     Output grows as n(n-1)/2, otherwise
  ;;     Output grows as n(n-1)
  ;;
  ;; There are an additional n edges when allow-loops is true.
  (assert (member graph-type '(directed-graph undirected-graph)))
  (let ((edge-generating-fn
	 (if (eq graph-type 'undirected-graph) #'combinations #'permutations)))
    (append (and (> (length vertices) 1)
		 (funcall edge-generating-fn vertices :length 2))
	    (and allow-loops (generate-loops vertices)))))

(defun max-edges (num-vertices graph-type allow-loops)
  "Return the maximum number of edges possible for a graph of
``graph-type'' with ``num-vertices'' vertices."
  (assert (member graph-type '(directed-graph undirected-graph)))
  (if (eq graph-type 'undirected-graph)
      (nth-triangular (if allow-loops num-vertices (1- num-vertices)))
      (* num-vertices (if allow-loops num-vertices (1- num-vertices)))))

(defun mk-random-graph (num-vertices num-edges
			&key (graph-type 'undirected-graph) allow-loops)
  "Return a random graph of type ``graph-type'' with ``num-vertices''
vertices and ``num-edges'' edges."
  (assert (member graph-type '(directed-graph undirected-graph)))
  (assert (<= num-edges (max-edges num-vertices graph-type allow-loops)))
  (let* ((vertices (loop repeat num-vertices collect (gensym)))
	 (edges (rnd-select-knuth (generate-all-edges
				   vertices
				   :graph-type graph-type
				   :allow-loops allow-loops)
				  num-edges)))
    (make-instance graph-type :data (list vertices edges))))

(defun generate-all-graphs-with-at-most-n-vertices
    (num-vertices &key (graph-type 'undirected-graph) allow-loops)
  "Return a list of all possible graphs of type ``graph-type'' with
``num-vertices'' or fewer vertices. Self-loops are included or
excluded based on the value of ``allow-loops''."

  ;; WARNING: The number of graphs grows exponentially with
  ;; num-vertices. You probably don't want to call this function with
  ;; num-vertices greater than about 4 or 5.
  (assert (member graph-type '(directed-graph undirected-graph)))
  (assert (< num-vertices (if (eq graph-type 'directed-graph)
			      (if allow-loops 4 5)
			      (if allow-loops 5 6))))
  ;; When graph-type is 'DIRECTED-GRAPH and allow-loops is true, the
  ;; number of graphs output by this function grows as:
  ;;     a(n) = Sum_{k=0,n} 2^(k^2)
  ;; In other words:
  ;;     a(0) = 1
  ;;     a(1) = 3
  ;;     a(2) = 19
  ;;     a(3) = 531
  ;;     a(4) = 66,067
  ;;     a(5) = 33,620,499
  ;;     a(6) = 68,753,097,235
  ;;     a(7) = 563,018,706,518,547
  ;;
  ;; You get the idea.
  ;;
  ;; When graph-type is 'DIRECTED-GRAPH and allow-loops is nil, output
  ;; grows as the only-slightly-less-horrendous
  ;;    a(n) = Sum_{k=0,n} 2^(k^2 - k)
  ;;
  ;; When graph-type is 'UNDIRECTED-GRAPH and allow-loops is
  ;; true, the number of graphs output by this function grows as
  ;;     a(n) = Sum_{k=1,n} 2^T(k)
  ;; where T are the triangular numbers, 0, 1, 3, 6, 10...
  ;; In other words:
  ;;     a(0) = 1
  ;;     a(1) = 3
  ;;     a(2) = 11
  ;;     a(3) = 75
  ;;     a(4) = 1,099
  ;;     a(5) = 33,867
  ;;     a(6) = 2,131,019
  ;;     a(7) = 270,566,475
  ;;
  ;; When graph-type is 'UNDIRECTED-GRAPH and allow-loops is nil,
  ;; the output grows as
  ;;    a(n) = 1 + Sum_{k=1,n} 2^T(k-1)
  ;;
  ;; See: http://oeis.org/A181388
  (loop for n upto num-vertices
     for vertices = (loop repeat n collect (gensym))
     for all-edges = (generate-all-edges vertices
					 :graph-type graph-type
					 :allow-loops allow-loops)
     append (loop for i upto (length all-edges)
	       for edge-combinations = (combinations all-edges :length i)
	       append (loop for edges in edge-combinations
			 collect (make-instance graph-type
						:data (list vertices edges))))))


;;; Lisp-unit Tests
(define-test isomorph-p-test
  ;; Ensure at least one non-trivial non-isomorph is tested. These
  ;; graphs have the same number of vertices and edges, and the same
  ;; sorted-degrees. This ensures that we enter the MATCH function in
  ;; isomorph-p.
  (assert-false (isomorph-p (mk-digraph '(a b c d e f g)
					'((a b) (b c) (c e) (c f) (d b) (g c)))
			    (mk-digraph '(a b c d e f g)
					'((a d) (c d) (d b) (d g) (f g) (g e)))))
  (dolist (graph (list* (mk-random-graph 10 25
					 :graph-type 'directed-graph
					 :allow-loops t)
                        (mk-random-graph 50 100
					 :graph-type 'directed-graph
					 :allow-loops t)
			;; Un-comment these for a slower but more
			;; thorough test. For reference, each one adds
			;; about 1.5 seconds to the test time on my
			;; old and crufty-but-trusty laptop.
			;;
			;; (mk-random-graph 50 500
			;;                  :graph-type 'directed-graph
			;;                  :allow-loops t)
			;; (mk-random-graph 100 200
			;;                  :graph-type 'directed-graph
			;;                  :allow-loops t)
			(generate-all-graphs-with-at-most-n-vertices
			 3
			 :graph-type 'directed-graph
			 :allow-loops t)))
    (assert-true (every (lambda (g) (isomorph-p graph g))
    			(if (< (length (vertices graph)) 5)
    			    (generate-all-isomorphs graph)
    			    (generate-random-isomorphs graph 20))))
    (assert-false (isomorph-p graph (add-edge '(foo bar) graph)))
    (assert-false (isomorph-p graph (add-edge '(foo a) graph)))
    (unless (null (edges graph))
      (assert-false (isomorph-p graph (remove-edge (first (edges graph)) graph))))))

(define-test flat-list-test
  (assert-equal nil (flat-list))
  (assert-equal '(1) (flat-list 1))
  (assert-equal '(1 2) (flat-list nil '(1 2) nil))
  (assert-equal '(1 2 3 4 5) (flat-list '(1) 2 nil '(3 4) 5)))

(define-test permutations-test
  (assert-equality #'set-set-equal '() (permutations '()))
  (assert-equality #'set-set-equal '((a)) (permutations '(a)))
  (assert-equality #'set-set-equal '((a b) (b a)) (permutations '(a b)))
  (assert-equality #'set-set-equal
		   '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
		   (permutations '(a b c)))
  (assert-equality #'set-set-equal
		   '((a b c d) (a b d c) (a c b d) (a c d b) (a d b c) (a d c b)
		     (b a c d) (b a d c) (b c a d) (b c d a) (b d a c) (b d c a)
		     (c a b d) (c a d b) (c b a d) (c b d a) (c d a b) (c d b a)
		     (d a b c) (d a c b) (d b a c) (d b c a) (d c a b) (d c b a))
		   (permutations '(a b c d))))

(define-test translate-test
  (assert-equal '() (translate '() '()))
  (assert-equal '() (translate '((a . b)) '()))
  (assert-equal '(a (b c) d) (translate '() '(a (b c) d)))
  (assert-equal '(z b) (translate '((a . newa)) '(z b)))
  (assert-equal '(newa newb) (translate '((a . newa) (b . newb)) '(a b)))
  (assert-equal '((newa newb) (newa newc) (newb newa) (newb newc) (newc newa) (newc newb))
		(translate '((a . newa) (b . newb) (c . newc))
			   '((a b) (a c) (b a) (b c) (c a) (c b))))
  (assert-equal '(newa (newb c) d)
		(translate '((a . newa) (b . newb)) '(a (b c) d)))
  (assert-equal '(z (d (newa f)) ((newb)))
		(translate '((a . newa) (b . newb)) '(z (d (a f)) ((b))))))

(define-test sorted-degrees-test
  (assert-equal '(3 2 1 1 1) (sorted-degrees (mk-graph   '(a b c d e) '((a b) (b c) (c d) (c e)))))
  (assert-equal '(3 2 1 1 1) (sorted-degrees (mk-digraph '(a b c d e) '((a b) (b c) (c d) (c e)))))
  (assert-equal '(4 3 3 2 2) (sorted-degrees (mk-graph   '(a b c d e) '((a b) (a c) (a d) (a e) (b c) (b d) (d e)))))
  (assert-equal '(4 3 3 2 2) (sorted-degrees (mk-digraph '(a b c d e) '((a b) (a c) (a d) (a e) (b c) (b d) (d e)))))
  (assert-equal '(5 3) (sorted-degrees (mk-graph   '(a b) '((a b) (a b) (b a) (b b)))))
  (assert-equal '(5 3) (sorted-degrees (mk-digraph '(a b) '((a b) (a b) (b a) (b b))))))

(define-test generate-all-isomorphs-test
  (assert-equality
   #'set-of-graphs-equal
   (mapcar (lambda (edges) (mk-digraph '(a b c) edges))
	   '(((a a) (a c) (b a) (c a) (c b))
	     ((b b) (b c) (a b) (c b) (c a))
	     ((c c) (c b) (a c) (b c) (b a))
	     ((a a) (a b) (c a) (b a) (b c))
	     ((b b) (b a) (c b) (a b) (a c))
	     ((c c) (c a) (b c) (a c) (a b))))
   (generate-all-isomorphs (mk-digraph '(a b c) '((a a) (a c) (b a) (c a) (c b))))))

(define-test generate-all-edges-test
  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '() :graph-type 'undirected-graph))
  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '() :graph-type 'undirected-graph :allow-loops t))
  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '() :graph-type 'directed-graph))
  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '() :graph-type 'directed-graph :allow-loops t))

  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '(a) :graph-type 'undirected-graph))
  (assert-equality
   #'set-set-equal
   '((a a))
   (generate-all-edges '(a) :graph-type 'undirected-graph :allow-loops t))
  (assert-equality
   #'set-set-equal
   '()
   (generate-all-edges '(a) :graph-type 'directed-graph))
  (assert-equality
   #'set-set-equal
   '((a a))
   (generate-all-edges '(a) :graph-type 'directed-graph :allow-loops t))

  (assert-equality
   #'set-set-equal
   '((a b))
   (generate-all-edges '(a b) :graph-type 'undirected-graph))
  (assert-equality
   #'set-set-equal
   '((a a) (a b) (b b))
   (generate-all-edges '(a b) :graph-type 'undirected-graph :allow-loops t))
  (assert-equality
   #'set-set-equal
   '((a b) (b a))
   (generate-all-edges '(a b) :graph-type 'directed-graph))
  (assert-equality
   #'set-set-equal
   '((a a) (a b) (b a) (b b))
   (generate-all-edges '(a b) :graph-type 'directed-graph :allow-loops t))

  (assert-equality
   #'set-set-equal
   '((a b) (a c) (b c))
   (generate-all-edges '(a b c) :graph-type 'undirected-graph))
  (assert-equality
   #'set-set-equal
   '((a a) (a b) (a c) (b b) (b c) (c c))
   (generate-all-edges '(a b c) :graph-type 'undirected-graph :allow-loops t))
  (assert-equality
   #'set-set-equal
   '((a b) (a c) (b a) (b c) (c a) (c b))
   (generate-all-edges '(a b c) :graph-type 'directed-graph))
  (assert-equality
   #'set-set-equal
   '((a a) (a b) (a c) (b b) (b a) (b c) (c c) (c a) (c b))
   (generate-all-edges '(a b c) :graph-type 'directed-graph :allow-loops t)))

(define-test mk-random-graph-test
  (flet ((num-edges-list (num-vertices graph-type allow-loops)
	   (let ((max-edges (max-edges num-vertices graph-type allow-loops)))
	     (cond ((zerop max-edges) (list 0))
		   ((= max-edges 1) (list 0 1))
		   (t (list 0 (1+ (random (1- max-edges))) max-edges))))))
    (dolist (graph-type '(directed-graph undirected-graph))
      (dolist (allow-loops '(t nil))
	(assert-eq 'ok (handler-case (mk-random-graph 0 1 :graph-type graph-type :allow-loops allow-loops)
			 (simple-error () 'ok)))
	(dolist (num-vertices (flat-list (alexandria:iota 10) 100))
	  (dolist (num-edges (num-edges-list num-vertices graph-type allow-loops))
	    (let ((graph (mk-random-graph num-vertices
					  num-edges
					  :graph-type graph-type
					  :allow-loops allow-loops)))
	      (assert-eql num-vertices (order graph))
	      (assert-eql num-edges (length (edges graph))))))))))

(define-test generate-all-graphs-with-at-most-n-vertices-test
  ;; Undirected graphs
  (let* ((all-zero-vertex-graphs (list (mk-graph '() '())))
	 (all-one-vertex-graphs
	  (append all-zero-vertex-graphs
		  (list (mk-graph '(a) '()))))
	 (all-one-vertex-graphs-with-loops
	  (append all-one-vertex-graphs
		  (list (mk-graph '(a) '((a a))))))
	 (all-two-vertex-graphs
	  (append all-one-vertex-graphs
		  (list (mk-graph '(a b) '())
			(mk-graph '(a b) '((a b))))))
	 (all-two-vertex-graphs-with-loops
	  (append all-one-vertex-graphs-with-loops
		  (list (mk-graph '(a b) '())
			(mk-graph '(a b) '((a a)))
			(mk-graph '(a b) '((a b)))
			(mk-graph '(a b) '((b b)))
			(mk-graph '(a b) '((a a) (a b)))
			(mk-graph '(a b) '((a a) (b b)))
			(mk-graph '(a b) '((a b) (b b)))
			(mk-graph '(a b) '((a a) (a b) (b b)))))))
    (assert-equality
     #'set-of-isomorphs-p
     all-zero-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 0
						  :graph-type 'undirected-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-zero-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 0
						  :graph-type 'undirected-graph
						  :allow-loops t))
    (assert-equality
     #'set-of-isomorphs-p
     all-one-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 1
						  :graph-type 'undirected-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-one-vertex-graphs-with-loops
     (generate-all-graphs-with-at-most-n-vertices 1
						  :graph-type 'undirected-graph
						  :allow-loops t))
    (assert-equality
     #'set-of-isomorphs-p
     all-two-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 2
						  :graph-type 'undirected-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-two-vertex-graphs-with-loops
     (generate-all-graphs-with-at-most-n-vertices 2
						  :graph-type 'undirected-graph
						  :allow-loops t)))

  ;; Directed graphs
  (let* ((all-zero-vertex-graphs (list (mk-digraph '() '())))
	 (all-one-vertex-graphs
	  (append all-zero-vertex-graphs
		  (list (mk-digraph '(a) '()))))
	 (all-one-vertex-graphs-with-loops
	  (append all-one-vertex-graphs
		  (list (mk-digraph '(a) '((a a))))))
	 (all-two-vertex-graphs
	  (append all-one-vertex-graphs
		  (list (mk-digraph '(a b) '())
                        (mk-digraph '(a b) '((a b)))
			(mk-digraph '(a b) '((b a)))
			(mk-digraph '(a b) '((a b) (b a))))))
	 (all-two-vertex-graphs-with-loops
	  (append all-one-vertex-graphs-with-loops
		  (list (mk-digraph '(a b) '())
                        (mk-digraph '(a b) '((a a)))
                        (mk-digraph '(a b) '((a b)))
			(mk-digraph '(a b) '((b a)))
			(mk-digraph '(a b) '((b b)))
                        (mk-digraph '(a b) '((a a) (a b)))
                        (mk-digraph '(a b) '((a a) (b a)))
			(mk-digraph '(a b) '((a a) (b b)))
			(mk-digraph '(a b) '((a b) (b a)))
			(mk-digraph '(a b) '((a b) (b b)))
			(mk-digraph '(a b) '((b a) (b b)))
			(mk-digraph '(a b) '((a a) (a b) (b a)))
			(mk-digraph '(a b) '((a a) (a b) (b b)))
                        (mk-digraph '(a b) '((a a) (b a) (b b)))
			(mk-digraph '(a b) '((a b) (b a) (b b)))
			(mk-digraph '(a b) '((a a) (a b) (b a) (b b)))))))
    (assert-equality
     #'set-of-isomorphs-p
     all-zero-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 0 :graph-type 'directed-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-zero-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 0
						  :graph-type 'directed-graph
						  :allow-loops t))
    (assert-equality
     #'set-of-isomorphs-p
     all-one-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 1 :graph-type 'directed-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-one-vertex-graphs-with-loops
     (generate-all-graphs-with-at-most-n-vertices 1
						  :graph-type 'directed-graph
						  :allow-loops t))
    (assert-equality
     #'set-of-isomorphs-p
     all-two-vertex-graphs
     (generate-all-graphs-with-at-most-n-vertices 2 :graph-type 'directed-graph))
    (assert-equality
     #'set-of-isomorphs-p
     all-two-vertex-graphs-with-loops
     (generate-all-graphs-with-at-most-n-vertices 2
						  :graph-type 'directed-graph
						  :allow-loops t))))
