\title{An engine for the game Saikoro}

@ Here we describe a program for playing the Saikoro game. We'll write
it as a vanilla C program that looks like this:

@c
@<Header files to include@>
@<Structs and typedefs@>
@<Global variables@>
@<Functions@>
@<The main program@>

@ We're gonna need the standard header files, plus a couple more for
generating random numbers.

@<Header files to include@>=
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

@ I don't really expect this program to work on anything else than the
standard 8x8 board, but it's always nice to define these kinds of constants:

@d ROWS 8
@d COLS 8

@ We'll represent the state of the game by an 8x8 array, each entry of
which holds either a number from 1 to 6, or 0 if a player has
previously visited that square. We'll also keep track of each player's
position.

@<Structs and typedefs@>=
typedef struct {
  int row;
  int col;
} Square;
  
typedef struct {
  int board[ROWS][COLS];
  Square black, white;
} Game;

@<Global variables@>=
Game game;

@ The positions of white and black will be marked on the board with
the 'W' and 'B' characters.

@d WHITE 'W'
@d BLACK 'B'

@ The player whose turn it is.

@<Global variables@>+=
char PLAYING = BLACK;

@ I anticipate that we will eventually write different playing
engines, each defined by a different strategy. That's why I begin by
defining three different strategies:

@<Global variables@>+=
enum strategy_t {RANDOM, IMMEDIATE, SEARCH};
enum strategy_t STRATEGY = IMMEDIATE;

@
@<The main program@>=
int main() {
  init_game();
  while(1) {
    show_board();
    if (! playing_can_move()) break;
    if (BLACK == PLAYING) {
      prompt_for_move();
      PLAYING = WHITE;
    }
    else {
      find_best_move(STRATEGY);
      PLAYING = BLACK;
    }
  }
  announce_winner();
}


@ We initialize the game by rolling the dice and placing the players.

@<Functions@>=
void init_game() {
  int row, col;
  for (row = 0; row < ROWS; row++) {
    for (col = 0; col < COLS; col++) {
      game.board[row][col] = roll_die();
    }
  }
  game.board[1][1] = 0;
  game.board[6][6] = 0;
  game.white.row = game.white.col = 1;
  game.black.row = game.black.col = 6;
}

int roll_die() {
  return ((double) rand() / (RAND_MAX+1.)) * 6 + 1;
}

@ Print the board to the console.
@<Functions@>+=
void show_board() {
  char board_as_string[] = 
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "|     |     |     |     |     |     |     |     |\n"
    "+-----+-----+-----+-----+-----+-----+-----+-----+\n";
  int row, col;
  for (row = 0; row < ROWS; row++) {
    for (col = 0; col < COLS; col++) {
      if (game.board[row][col] > 0) {
        board_as_string[two_to_one_dim(row, col)] = '0' + game.board[row][col];
      }
    }
  }
  printf("%s", board_as_string);
}

int two_to_one_dim(int row, int col) {
  return 1503 - (row * 200) + 6 * col;
}

@ We need a routine that returns an array of all squares that a given
player can reach. We will define this recursively: a player can reach
all adjacent squares with value 1, plus all squares reachable from
those when the numbers on the board have been decremented.

@d MAX_MOVES 1000

@<Functions@>+=
Square *find_legal_moves(char player, Game game) {
  
}

@
@<Functions@>+=
int playing_can_move() {
  return 0;
}

void prompt_for_move() {
 return;
}

void find_best_move(strategy_t strategy) {
  return;
}

void announce_winner() {
  return;
}
