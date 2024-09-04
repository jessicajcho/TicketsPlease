# Tickets Please: Alert Level Red

Thank you for your interest in *Tickets Please: Alert Level Red*!

In this game, you're a Cornell ticket collector who has to follow 
the rules of valid documentation to let in only allowed individuals 
to Cornell events. But be careful of sneaky people, lest your dogs 
die or worse!


The instructions below assume that you do not yet have OCaml or VS Code. If you already have the base 
configuration of OCaml suggested for Cornell CS 3110 but not VS Code, skip to step 2 under **OCaml Installation Instructions**. 
If you already have OCaml and VS Code, skip to step 4 under 
**OCaml Installation Instructions** to install the `ANSITERMINAL` package.

If you already have this configuration of OCaml, VS Code, and `ANSITERMINAL` and are viewing this page 
from Github, please skip to the instructions under **Instructions to Play the Game** to begin playing the game.

If you have already downloaded this game to a folder on your computer, please skip to instruction 2 under **Instructions to Play the Game**.

## OCaml Installation Instructions

1. Install OCaml per the directions found in the beginning of
[*OCaml Programming: Correct + Efficient + Beautiful*](https://cs3110.github.io/textbook/chapters/preface/install.html)
    - Install using the correct directions for your operating system
    - Follow the instructions pertinent to Cornell's *CS 3110* course, including the
    instructions under headers:
      1. Installing OCaml
      2. Unix Development Environment 
      3. Install OPAM
      4. Initialize OPAM
      5. Create an OPAM switch 
      6. Double Check OCaml

2. Install `VS Code` per the directions found starting at *Visual Studio Code* in 
[*OCaml Programming: Correct + Efficient + Beautiful*](https://cs3110.github.io/textbook/chapters/preface/install.html). The sections you should complete include:
    1. Visual Studio Code 
    2. Double Check VS Code 
    3. VS Code Settings 

3. Install `ANSITERMINAL`
    1. Update OPAM once more by running `$ opam update` in your computer's terminal
  (Note: the `$` represents the terminal prompt and is not part of the input command)
        - On MacOS or Linux, this should be your standard terminal
        - On Windows, this should be the ubuntu terminal. This app can be downloaded from the Microsoft Store. The standard ubuntu app works fine.
        - Do not be alarmed if this command takes a long time
        - If you are prompted after this command to upgrade already installed packages, 
        enter `$ opam upgrade` in the same terminal
    2. Install the package by entering `$ opam install ANSITerminal` in your terminal


## Instructions to Play the Game 

(NOTE: This section assumes you have a valid Github account.)

1. Clone this repository (Mac)
    1. Choose a folder in your filesystem in which you want to save the game.
    2. Navigate to this folder using your terminal. On MacOS or Linux, use the standard terminal application.
    3. Enter the command `git clone git@github.coecis.cornell.edu:[YOUR NETID]/cs3110_final_project.git`, where the aforementioned link comes from the green `Code` tab in this repository. This link assumes you're using SSH for git, but HTTPS is also fine. Make sure to replace [YOUR NETID] with your netid.
    4. Alternatively: If cloning the repo does not work, you can download the zip file directy from this online git repository using the green `Code` button. Just unzip the file once it's placed in the correct directory.
    
1. Clone this repository (Windows)
    1. Choose a folder in your ubuntu filesystem in which you want to save the game files.
    2. Copy the path of the folder and, in ubuntu, use the command `cp [current path of folder] [desired path of folder]` to bring the folder into ubuntu.
    3. Enter the command `git clone git@github.coecis.cornell.edu:[YOUR NETID]/cs3110_final_project.git`, where the aforementioned link comes from the green `Code` tab in this repository. This link assumes you're using SSH for git, but HTTPS is also fine. Make sure to replace `[YOUR NETID]` with your netid.
    4. Alternatively: If cloning the repo does not work, you can download the zip file directy from this online git repository using the green `Code` button. Open a folder in VSCode and drag the game folder into the explorer on the left. Once there, use the ubuntu command `unzip` to unzip the folder.

2. Run the game and play! 
    1. Open VS Code and select `New Terminal` from the `Terminal` tab at the top of the application. Optionally, you could also use 
    any terminal of your choice that has access to you filesystem and the ability to run OCaml code. 
    2. Navigate using this terminal to the directory that contains all of the code from the repository 
    you just cloned (i.e. make sure you're in the directory where this file is on your computer)
    3. Type  `make play` into the VS Code terminal
    4. Enjoy the game!
