import time
import os
from kivy.app               import App
from kivy.uix.label         import Label
from kivy.uix.textinput     import TextInput
from kivy.uix.floatlayout   import FloatLayout
from kivy.clock             import Clock

class Board(FloatLayout):
    def __init__(self, **kwargs):
        super(Board, self).__init__(**kwargs)
        self.text_inputs    = []
        grid = self.ids["grid"]
        self.board_size = 9
        for i in range(81):
            text_input = Cell()
            grid.add_widget(text_input)
            self.text_inputs.append(text_input)

    def get_value(self, row, col):
        text  = self.text_inputs[self.board_size * row + col].text
        return int(text) if len(text) > 0 else 0

    def solve(self):
        values = [[self.get_value(row, col) for col in range(self.board_size)] for row in range(self.board_size)]
        self.write_to_file(values)
        os.system("./sudoku_solver board.txt")
        if not self.read_output():
            error_message = ErrorMessage()
            self.error_message = error_message
            self.add_widget(error_message)
            Clock.schedule_once(self.remove_error_message, 2)
    
    def write_to_file(self, values):
        target = open("board.txt", 'w')
        for i in range(len(values)):
            for j in range(len(values[i])):
                if(j!=len(values[i])-1):
                    target.write(str(values[i][j])+" ")
                else:
                    target.write(str(values[i][j]))
            target.write("\n")
        target.close()

    def gen_more(self):
        os.system("./sudoku_solver -n")
        if not self.read_output():
            error_message = ErrorMessage()
            self.error_message = error_message
            self.add_widget(error_message)
            Clock.schedule_once(self.remove_error_message, 2)

    def read_output(self):
        with open("answer.txt") as target:
            row = 0
            for line in target:
                if line[0] == 'n':
                    return False
                for col in range(self.board_size):
                    self.text_inputs[self.board_size * row + col].text = line[col]
                row += 1
        return True

    # Change size of the board
    def change(self):
        grid = self.ids["grid"]
        for i in range(len(self.text_inputs)):
            grid.remove_widget(self.text_inputs[i])
        self.text_inputs = []
        if (self.board_size == 9):
            self.board_size = 4
        else:
            self.board_size = 9

        self.ids["grid"].rows = self.board_size
        self.ids["grid"].cols = self.board_size
        for i in range(self.board_size * self.board_size):
            text_input = Cell()
            grid.add_widget(text_input)
            self.text_inputs.append(text_input)

    # Remove old error message
    def remove_error_message(self, dt):
        self.remove_widget(self.error_message)

    # Remove all values on the board
    def clear(self):
        for text_input in self.ids["grid"].children:
            text_input.text = ""

class Cell(TextInput):
    pass


class ErrorMessage(Label):
    pass


class SudokuApp(App):
    def build(self):
        return Board()


if __name__ == '__main__':
    SudokuApp().run()