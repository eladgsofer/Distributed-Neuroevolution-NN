Youtube link: https://www.youtube.com/watch?v=gjLjRERK-dA

This application simulated a hunter rabbit chase, using evolutinal neural network.

Install INSTRACTIONS:
1.Do git clone https://github.com/eladsofer879/EralngNN.git
2.Open config.hrl file and config your nodes name as node1/node2/mode3/king and ip's.
3.Open 4 terminals.
4.Navigate in the terminals to the library with the files of the project.
5.Type in each terminal: "erl -make"
6.Initialize 3 terminals with names node1/2/3 and identical setcookie.
7.type: "test:slave()."
8.For the final terminal give the name "king" and the same setcookie.
9.Type in the king terminal "test:master()."


Description:
Perform a learning mechanism in order to catch a running cat.
The learning process is being made via an evolutional neural network's algorithm. 
In order to alleviate the calculations, the learning process is being distributed into 4 nodes. Each node has a population of genes which it responsible for it’s evolution
Fault tolerant system – in case a node falls, the system redistribute the workload between the live nodes.
Each node is consisted of several components:
 Master Server – OTP gen server
Population FSM – OTP gen statem
Agents Pool – OTP gen servers
Agents supervisor – OTP Supervisor

