required_states = ['accept', 'reject', 'init']

class TuringMachine(object):
    
    def __init__(self, sigma, gamma, delta):

        self.sigma = sigma
        self.gamma = gamma
        self.delta = delta

        self.state = None
        self.tape = None
        self.head_position = None
        return
        
    def initialize(self, input_string):
        
        for char in input_string:
            assert char in self.sigma

        self.tape = list(input_string)
        self.state = 'init'
        self.head_position = 0
        return

    def simulate_one_step(self, verbose=False):

        if self.state in ['accept', 'reject']:
            print "# %s " % self.state
        
        cur_symbol = self.tape[self.head_position]

        transition = self.delta[(self.state, cur_symbol)]
                                 
        if verbose:
            self.print_tape_contents()
            template = "delta({q_old}, {s_old}) = ({q}, {s}, {arr})"
            print(template.format(q_old=self.state, 
                                  s_old=cur_symbol,
                                  q=transition[0],
                                  s=transition[1],
                                  arr=transition[2])
            )
        
        self.state                    = transition[0]
        self.tape[self.head_position] = transition[1]
        
        if(transition[2] == 'left'):
            self.head_position = max(0, self.head_position - 1)
        else:
            assert(transition[2] == 'right')
            if self.head_position == len(self.tape) - 1:
                self.tape.append('#')
            self.head_position +=1
        if verbose:
            self.print_tape_contents()
        return

    def print_tape_contents(self):
        
        formatted = ''.join(char if i != self.head_position else '[%s]' % char
                            for i, char in enumerate(self.tape))
        print(formatted)
    
    def run(self, input_string, verbose=False):
        self.initialize(input_string)
        while self.state not in ['reject', 'accept']:
            self.simulate_one_step(verbose)
        return str(self.tape)
            

