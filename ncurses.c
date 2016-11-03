#include <ncurses.h>

void term_close() {
	endwin();
}

void term_refresh() {
	refresh();
}

void term_clear() {
	clear();
}

void term_print(int x, int y, char *string) {
	mvaddstr(y, x, string);
}

void term_getsize(int *width, int *height) {
	getmaxyx(stdscr, *height, *width);
}

int term_getchar() {
	return getch();
}

/*
cf pkg_ncurses.ads:
   COLOR_RED     : constant := 1;
   COLOR_GREEN   : constant := 2;
   COLOR_YELLOW  : constant := 3;
   COLOR_BLUE    : constant := 4;
   COLOR_MAGENTA : constant := 5;
   COLOR_CYAN    : constant := 6;
   COLOR_WHITE   : constant := 7;
   COLOR_BLACK   : constant := 8;
*/
void term_setcolor(int color) {
	color_set(color, NULL);
}

// Reverse attribute
// value is boolean
void term_setreverse(int value) {
	value ? attron(A_REVERSE) : attroff(A_REVERSE);
}

void term_init() {
	// We want all these settings
	initscr();
	noecho();
	start_color();
	curs_set(FALSE);
	attron(A_BOLD);
	nodelay(stdscr, TRUE);
	keypad (stdscr, TRUE);

	// Set a basic palette of colors
	init_pair(1, COLOR_RED, COLOR_BLACK);
	init_pair(2, COLOR_GREEN, COLOR_BLACK);
	init_pair(3, COLOR_YELLOW, COLOR_BLACK);
	init_pair(4, COLOR_BLUE, COLOR_BLACK);
	init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
	init_pair(6, COLOR_CYAN, COLOR_BLACK);
	init_pair(7, COLOR_WHITE, COLOR_BLACK);
	init_pair(8, COLOR_BLACK, COLOR_BLACK);

	// white color by default
	term_setcolor(1);
}