#include <stddef.h>
#include <stdint.h>

unsigned int LogicGrowsOnTrees_Queens_count_solutions(
    unsigned int size,
    unsigned int number_of_queens_remaining,
    unsigned int row,
    uint64_t occupied_rows,
    uint64_t occupied_columns,
    uint64_t occupied_negative_diagonals,
    uint64_t occupied_positive_diagonals,
    void (*pushValue)(unsigned int,unsigned int),
    void (*popValue)(),
    void (*finalizeValue)()
) {
    if(occupied_rows & 1) {
        return
            LogicGrowsOnTrees_Queens_count_solutions(
                size,
                number_of_queens_remaining,
                row+1,
                occupied_rows >> 1,
                occupied_columns,
                occupied_negative_diagonals >> 1,
                (occupied_positive_diagonals << 1) + ((occupied_positive_diagonals >> 63) & 1),
                pushValue,
                popValue,
                finalizeValue
            );
    }
    unsigned int
        number_of_solutions = 0,
        column_bit = 1,
        column = 0;
    uint64_t blocked = occupied_columns | occupied_negative_diagonals | occupied_positive_diagonals;
    for(column = 0; column < size; ++column, column_bit <<= 1) {
        if(!(column_bit & blocked)) {
            #ifdef __GNUC__
            if(__builtin_expect(pushValue != NULL,0)) {
            #else
            if(pushValue != NULL) {
            #endif
                (*pushValue)(row,column);
            }
            if(number_of_queens_remaining == 1) {
                #ifdef __GNUC__
                if(__builtin_expect(finalizeValue != NULL,0)) {
                #else
                if(finalizeValue != NULL) {
                #endif
                    (*finalizeValue)();
                }
                number_of_solutions += 1;
            } else {
                number_of_solutions +=
                    LogicGrowsOnTrees_Queens_count_solutions(
                        size,
                        number_of_queens_remaining-1,
                        row+1,
                        occupied_rows >> 1,
                        occupied_columns | column_bit,
                        (occupied_negative_diagonals | column_bit) >> 1,
                        ((occupied_positive_diagonals | column_bit) << 1) + ((occupied_positive_diagonals >> 63) & 1),
                        pushValue,
                        popValue,
                        finalizeValue
                    );
            }
            #ifdef __GNUC__
            if(__builtin_expect(popValue != NULL,0)) {
            #else
            if(popValue != NULL) {
            #endif
                (*popValue)(row,column);
            }
        }
    }
    return number_of_solutions;
}
