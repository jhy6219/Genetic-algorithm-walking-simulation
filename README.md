# Simulation of the Physical Movement for Machine Learning with R

Recently, machine learning algorithms are used popularly in the engineering field, and simulation of the specific situation becomes one of the important processes. 
In this talk, we propose an environment for simulation of the physical movement in robots with R. 
We carried out the optimization of robot gait which is a major issue in the robotics area. As an optimization algorithm, GA (Genetic Algorithm) was used. In our physical simulation, we considered two options. The first option is about position and velocity when forces
are applied to the body. We solved this dynamic equation of a robot's body placed in 2-dimensional space using the 3rd order Runge-Kutta method. The second option is for constraining the position and velocity when a body contacts the ground. The sequences of the robot's body position were visualized with animation package in R. This virtual robot has a body, and five parameters determine its walking trajectory. GA package in R was used to optimize these parameters. We successfully get some values that enable robots to walk steady and fast. Through this study, we expect simulation in robotics engineering area can be conducted with R as well.


New generations walk faster than old generations.

<1st generation>

![walkingg1](https://user-images.githubusercontent.com/47768004/67558923-e1d93c00-f752-11e9-908e-6e39b1785826.gif)


<fifth generation>

![newwalkingg5](https://user-images.githubusercontent.com/47768004/67558885-cf5f0280-f752-11e9-9a97-c554ad76d575.gif)

<20th generation>
![newwalkingg20](https://user-images.githubusercontent.com/47768004/67558740-80b16880-f752-11e9-87c7-324fc27866b4.gif)

<100th generation>

![newwalkingg100](https://user-images.githubusercontent.com/47768004/67558788-9c1c7380-f752-11e9-8554-7e17c44a19d4.gif)

Did GA find the farthest walking robot? 
A red dot is distance of  100-generation robot for 5  seconds. 
Therefore, we reached the global maximum using Ga.

![GIF](https://user-images.githubusercontent.com/47768004/67558935-e6055980-f752-11e9-9cf0-16c2aae1da08.gif)

