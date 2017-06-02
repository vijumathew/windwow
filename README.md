# Windwow #

Windwow is an Emacs package with a small collection of functions for saving and loading window and buffer configurations. 

## Inspiration ##

This package simplifies workspace management for Emacs. With just a few functions we can save window arrangements, load those arrangements, and do the same for buffers. These values are easily persisted because they are simple data structures - there are functions used internally that translate a window arrangement to a list of split and switch commands. 

## Functions ##

A buffer configuration is a list of buffers and a window configuration is an arrangement of windows in a frame. Right now window configurations created only with split and switch commands are supported. These functions can be called interactively (via `M-x`) or from keybindings.

### Buffer ###
  * `save-buffer-list` - saves current buffers and prompts for name
  * `load-buffer-list` - loads a previously saved buffer list
  * `load-buffer-from-list` - loads a buffer from a saved buffer list

### Window ###
  * `save-window-configuration` - saves current window configuration
  * `load-window-configuration` - loads a previously saved window configuration

### Buffer and window ###
  * `load-window-configuration-and-buffer-list` - loads a window configuration and a buffer list 

## Roadmap ##
  * Support frames with manually resized windows
  * Minor mode
  * More efficient reconstruction algorithm
  * More documentation
