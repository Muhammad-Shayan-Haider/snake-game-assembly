# snake-game-assembly
A typical snake game developed in IAPX88 architecture assembly.

**Functionalities**:
1.	A snake that automatically moves left, right, up and down on the screen.
2.	Initial size of snake is 20 characters with head of the snake represented by a different character as compared to its body.
3.	The arrow keys are used to determine the direction of snake.
4.	If the snake’s head touches the border of screen the players loses one life. Initially the player has three lives. After losing all three lives the game is over.
5.	If the snake eats a fruit the size of snake is increased by four characters. 
6.	If the snake does not eat the fruit, the fruit remains on the screen.
7.	After the fruit is eaten by the snake, the next fruit immediately appears on a random location on the screen.
8.	Maximum size of snake can be 240 characters.
9.	If the snake does not reach the maximum size in 4 minutes one life of player will end.
10.	If the snake reaches the maximum size in 4 minutes the player earns some points and game ends with an appropriate message.
11.	After every 20 seconds the speed of snake is twice as the previous speed.
12.	Show on display the remaining and total lives.
13.	Show on display the time remaining and when the time is reset. 
14.	If the snake touches itself one life of player will end.

**How to run**:

DosBox is required to run this <code>.asm</code> file.
1. Open DosBox.
2. Mount the directory where <code>snake.asm</code> file is located i.e. by typing command <code> mount alias drive:\folder </code> where folder is the folder which contains <code>snake.asm</code> file.
3. Type <code> alias: </code>. The drive will be mounted after this command.
4. Run the command i.e. <code> nasm snake.asm -o snake.com </code>. snake.com is the output file generated.
5. Now type <code> snake.com </code> to run the game. Enjoy!!

Screenshots are attached for reference.

![image](https://user-images.githubusercontent.com/60185211/121808838-d44c2f00-cc73-11eb-82fc-db4210aa42a5.png)
![image](https://user-images.githubusercontent.com/60185211/121808874-f0e86700-cc73-11eb-964b-df9a1d434df8.png)

