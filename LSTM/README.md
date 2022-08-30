# Long Short-Term Memory (LSTM)
This is an artificial neural network, whose architecture inspired many state of the art architectures that build upon the idea of memory. This is a special kind of Recurrent Neural Network (RNN) where LSTM's attempt to store and refer to memory over a long period of time. The idea here is to connect historical information to the present day task.

# Gates
There are three gates in an LSTM; I go over these gates in more detail in this [YouTube Video](https://www.youtube.com/watch?v=rmxogwIjOhE&lc=UgxmxcNVnevS5eDUhpR4AaABAg). Essentially, these gates make up the neural network architecture.
- Forget Gate : Decides which information needs more attention and which information can be ignored.
- Input Gate : Updates the cell status
- Output Gate : Determines the value of the next hidden state (contains information on previous inputs)
