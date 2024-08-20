# GUI (Graphics User Interface)

**CSIM** comes with _Graphics USer Interface_ based on **Python** and [Orchid](https://github.com/hcasse/Orchid).

To run it, move to `python` directory and type:
```sh
$ make run NUM=number
```

With _number_ one of sample in `samples` directory.

## CSIM UI Structure

In order to have a display of the IO components (like LED or button), the GUI associates a Python component with IO **CSIM** components.
These Python components takes in charge the task of displaying the component in the GUI and inetracting with the user.

The Python module `csimui` (in directory `python`) defines resources to build the GUI and interface IO components (class `components.IOComponent`). This class defines two methods to interact with GUI and in turn to interact with CSIM component:

* `install`(_canvas_) allows to display the IO Component on an the **ORchid** SVG Canvas. The SVG allows to simply modify the drawing of the component to perform animation. In addition, events can be hung to the SVG element to detect user interaction.
* `update`() is called by the GUI each time the component can update itself from the **CSIM** component.

## Interacting with CSIM

The interaction with **CSIM** IO component is handed by 2 functions of `csim_iocomp_t`:

* `get_state`(_instance_, _data_) - get the state from IO component.
* `set_state`(_instance_, _data_) - set the satte of IO component.

_data_ are an array of `uint32_t` which size depends on the IO component. For example, for LED, _data_ contains 1 element which is the ligh state of the LED: 0 turned off, 1 turned on. For the button, there is also one `uint32_t`element that may be 0 (button released) or 1 (button pressed).

With each IO component, there is a Python component declared in the file `csimui/`_component_`.py`and which class is simply named `Component` and that inherit from `csimui.components.IOComponent`. The GUI uses this path to retrieve the Python component associated with the **CSIM** component.

## Examples

For example, the LED component is found in file `csimui/led.py`. Its display is obtained from `csimui/led.svg` that is loaded and installed in the GUI canvas. Each time `update` method is called, this class get the state from the matching **CSIM** component, with function `get_state`, and change the lookup of the LED SVG accordingly.

For the button, we have the same but SVG file `button.svg`. Moreover, it adds event handler to the button SVG to detect press and release events using **Orchid** API. When such an event arises, it calls function `set_state` to change the state of **CSIM** component and uses **Orchid** to update its display.

## Inserting SVG files in the canvas

In order to have access to elements composing the SVG picture, it has to be inserted as sub-elements of the SVG of the canvas. To do this, the file is stripped from its `<svg>` tag by the load function `csimui.util.load_svg`(_path_). Yet, it may contained elements with `id` that will be in conflict if several SVG images ared used in canvas. To avoid this, `id` attributes and their references can be prefixed with `{id}` that will be replaced by a value proper to each IO component (as obtained by `orchid.svg.Shape.get_id()`). These IDs can then be used to customize the lookup of the SVG image by changing attributes, adding or removing content, etc. The stripped SVG images can inserted using `orchid.svg.Canvas.content`(_image_).

## Passing parameters from YAML

`.yaml` files are used to build the board but additional parameters can be added to the component description. These parameters are passed to the IO component at construction time. They depends on the type of IO component.

For example, the following parameters can be passed:

* `x` -- X position in pixels (LED and button),
* `y` -- Y position in pixels (LED and button),
* `color` -- HTML ligth color (only for LED).




