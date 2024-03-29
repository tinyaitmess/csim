#!/usr/bin/env python3

import csim

board = csim.new_board("myboard", None)
print(board)

csim.run(board, 10)

csim.delete_board(board)
