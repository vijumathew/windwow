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

## Using Windwow ##

Here is an example showing how to use some of the aforementioned functions:

1. Switch to a frame with one window. Create a window arrangement with split and switch commands; these commands are `split-window-below` (`C-x 2`), `split-window-right` (`C-x 3`), and `other-window` (`C-x o`). Switch to a different buffer in each of these windows. 
2. Now save this configuration with `windwow-save-window-arrangement` and `windwow-save-buffer-list`.
3. Close all the windows except the current one with `delete-other-windows` (`C-x 1`).
4. Now restore the window arrangement and buffer list with `windwow-load-window-arrangement-and-buffer-list`. The windows previously created should be restored in the proper arrangement with the corresponding buffers loaded inside.

Unfortunately manually resized windows are not supported at the moment.

## Roadmap ##
  * Support frames with manually resized windows
  * Minor mode
  * More efficient reconstruction algorithm
  * More documentation
