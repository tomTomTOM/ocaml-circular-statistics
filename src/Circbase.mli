(** Circbase 
    Module with utility functions common to many other modules.
    Also useful to library user for generating bins and comparing
    circular values *)

(** {6 Basic comparison operators} *)

(** (%) + modulo operator *)
val (%) : float -> float
(** Always returns positive remainders. *)

(** Circular less-than *)
val (@<) : float -> float -> bool
(** [a @< b] returns true if a is circularly less than b.
    A phase is 'less' than another if the 
    course from (a % 2pi) to (b % 2pi) going
    counterclockwise is less than pi radians. *)

(** Circular less-than-or-equal-to *)
val (@<=) : float -> float -> bool
(** [a @<= b] Same as circular less-than,
    also true if circularly equal *)

(** Circularly equal *)
val (@=) : float -> float -> bool
(** [a @= b] retuns true if (a % 2pi) = (b % 2pi) *)

    
(** Circular greater-than *)
val (@>) : float -> float -> bool
(** [a @> b] returns true if a is circularly greater than b *)

(** Circular greater-than-or-equal-to *)
val (@>=) : float -> float -> bool
(** [a @>= b] returns true if a is circularly greater than
    or equal to b *)


(** {6 Arithmetic operators} *)

(** Circular difference *)
val (@-) : float -> float -> float
(** [a @- b] returns the shortest phase distance between
    a and b.  Positive if a @> b. In some situations it is 
    more useful when taken as positive-modulo 2pi 
    (e.g. when taking the half-way point between a and b) *)

(** Circular sum *)
val (@+) : float -> float -> float
(** [a @+ b] returns the sum of phases a and b, modulo'd
    into the range [-pi, pi) *)

(** {6 Array creation} *)

val circspace : 
  ?offset:float -> 
  ?halfshift:bool-> 
  ?start:float -> ?stop:float -> 
  int -> float array
(** [circspace ~offset:0. ~halfshift:false ~start:0. ~stop:pi2 n]





















