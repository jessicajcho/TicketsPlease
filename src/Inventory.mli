(** This module handles the inventory of a player as well as the shop in
    the game. *)

open State
(* The inventory file will be used to process the shop aspect of the
   game. The game will automatically pay for rent at the beginning of
   the shop sequence. In the case that the player does not have enough
   cash, assets may be reposessed. The ideal situation would be where
   each item has some impact in the actual gameplay. The player may also
   take care of dogs and grandparents on life support.*)

type item
(** [item] represents an object that the player can purchase. *)

val shop : t -> t
(** Handles the functionality and printing of the shop where the player
    can buy items. *)
