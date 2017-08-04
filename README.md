# Windwow #

Windwow is an Emacs package with a small collection of functions for saving and loading window arrangements and buffer configurations. 

## Inspiration ##

This package simplifies workspace management for Emacs. With just a few functions we can save window arrangements, load those arrangements, and do the same for buffers. These values are easily persisted because they are simple data structures - there are functions used internally that translate a window arrangement to a list of split and switch commands. 

## Functions ##

A buffer configuration is a list of buffers and a window arrangement is an arrangement of windows in a frame. Right now window arrangements created only with split and switch commands are supported. These functions can be called interactively (via `M-x`) or from keybindings.

### Buffer ###
  * `windwow-save-buffer-list` - saves current buffers and prompts for name
  * `windwow-load-buffer-list` - loads a previously saved buffer list
  * `windwow-load-buffer-from-list` - loads a buffer from a saved buffer list

### Window ###
  * `windwow-save-window-arrangement` - saves current window arrangement
  * `windwow-load-window-arrangement` - loads a previously saved window arrangement

### Buffer and window ###
  * `windwow-load-window-arrangement-and-buffer-list` - loads a window arrangement and a buffer list 

## Roadmap ##
  * Support frames with manually resized windows
  * Minor mode
  * More efficient reconstruction algorithm
  * More documentation
