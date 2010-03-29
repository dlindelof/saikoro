#define ROWS 8
#define COLS 8 \

#define WHITE 'W'
#define BLACK 'B' \

/*1:*/
#line 6 "./saikoro.w"

/*2:*/
#line 15 "./saikoro.w"

#include <stdio.h> 
#include <stdlib.h> 
#include <math.h> 

/*:2*/
#line 7 "./saikoro.w"

/*5:*/
#line 32 "./saikoro.w"

struct player_position{
int row;
int col;
};

struct{
int board[ROWS][COLS];
struct player_position black,white;
}game;

/*:5*//*7:*/
#line 51 "./saikoro.w"

char PLAYING= BLACK;

/*:7*//*8:*/
#line 58 "./saikoro.w"

enum strategy_t{RANDOM,IMMEDIATE,SEARCH};
enum strategy_t STRATEGY= IMMEDIATE;

/*:8*/
#line 8 "./saikoro.w"

/*10:*/
#line 84 "./saikoro.w"

void init_game(){
int row,col;
for(row= 0;row<ROWS;row++){
for(col= 0;col<COLS;col++){
game.board[row][col]= roll_die();
}
}
game.board[1][1]= 0;
game.board[6][6]= 0;
game.white.row= game.white.col= 1;
game.black.row= game.black.col= 6;
}

int roll_die(){
return((double)rand()/(RAND_MAX+1.))*6+1;
}

/*:10*//*11:*/
#line 103 "./saikoro.w"

void show_board(){
char board_as_string[]= 
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
int row,col;
for(row= 0;row<ROWS;row++){
for(col= 0;col<COLS;col++){
if(game.board[row][col]> 0){
board_as_string[two_to_one_dim(row,col)]= '0'+game.board[row][col];
}
}
}
printf("%s",board_as_string);
}

int two_to_one_dim(int row,int col){
return 1503-(row*200)+6*col;
}


/*:11*//*12:*/
#line 156 "./saikoro.w"

int playing_can_move(){
return 0;
}

void prompt_for_move(){
return;
}

void find_best_move(strategy_t strategy){
return;
}

void announce_winner(){
return;
}/*:12*/
#line 9 "./saikoro.w"

/*9:*/
#line 63 "./saikoro.w"

int main(){
init_game();
while(1){
show_board();
if(!playing_can_move())break;
if(BLACK==PLAYING){
prompt_for_move();
PLAYING= WHITE;
}
else{
find_best_move(STRATEGY);
PLAYING= BLACK;
}
}
announce_winner();
}


/*:9*/
#line 10 "./saikoro.w"


/*:1*/
