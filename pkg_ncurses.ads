package pkg_ncurses is
   COLOR_RED     : constant := 1;
   COLOR_GREEN   : constant := 2;
   COLOR_YELLOW  : constant := 3;
   COLOR_BLUE    : constant := 4;
   COLOR_MAGENTA : constant := 5;
   COLOR_CYAN    : constant := 6;
   COLOR_WHITE   : constant := 7;
   COLOR_BLACK   : constant := 8;

   ARROW_RIGHT : constant := 261;
   ARROW_DOWN  : constant := 258;
   ARROW_LEFT  : constant := 260;
   ARROW_UP    : constant := 259;

   procedure term_init;
   pragma Import (C,term_init,"term_init");

   procedure term_close;
   pragma Import (C,term_close,"term_close");

   procedure term_refresh;
   pragma Import (C,term_refresh,"term_refresh");

   procedure term_clear;
   pragma Import (C,term_clear,"term_clear");

   procedure term_setreverse(bool : integer);
   pragma Import (C,term_setreverse,"term_setreverse");

   procedure term_print(x   : integer; 
                        y   : integer; 
                        str : string);
   pragma Import (C,term_print,"term_print");

   procedure term_getsize(width  : out integer;
                          height : out integer);
   pragma Import (C,term_getsize,"term_getsize");

   procedure term_setcolor(color : positive);
   pragma Import (C,term_setcolor,"term_setcolor");

   function term_getchar return integer;
   pragma Import (C,term_getchar,"term_getchar");
end pkg_ncurses;