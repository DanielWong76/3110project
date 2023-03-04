type grid = {
  mines : int * int list;  (** (a,b) represents the coordinates of a mine *)
  opened : int * int list;
      (** (a,b) indicates where the player has revealed a tile *)
  dimensions : int * int;  (** (a,b) represents the grid is of size a*b *)
}
