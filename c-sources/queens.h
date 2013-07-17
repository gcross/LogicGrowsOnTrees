unsigned int LogicGrowsOnTrees_Queens_count_solutions(
    unsigned int size,
    unsigned int number_of_queens_remaining,
    unsigned int row,
    uint64_t occupied_rows,
    uint64_t occupied_columns,
    uint64_t occupied_negitive_diagonals,
    uint64_t occupied_positive_diagonals,
    void (*pushValue)(unsigned int,unsigned int),
    void (*popValue)(),
    void (*finalizeValue)()
);
