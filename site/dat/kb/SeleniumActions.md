# How to Quickly and Easily Write Selenium Scripts on Taurus

_Author: Yulia Shepeleva_

Load testing web applications has become common practice, with open-source [Apache JMeter](https://www.blazemeter.com/jmeter-load-testing?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium) being the most popular load testing tool. JMeter works with different protocols, including HTTP. However, due to its nature, JMeter is not able to perform all browser-supported actions, and it also doesn't execute the javascript found in the HTML page. Therefore, we need a tool that can operate on a user interface level of a real browser window.

The solution is [Selenium WebDriver](https://www.blazemeter.com/blog/how-automate-testing-using-selenium-webdriver-jenkins-and-allure?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium). However, Selenium's technology requires you write the script in one of the supported programing languages, and this might be difficult for users without a technical background. The solution is to use Selenium with [Taurus DSL](http://gettaurus.org/docs/Selenium/?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium), which provides a simplification for user web page level scripting, with the concept of 'simple UI actions'.

Taurus uses simple configuration files in [YAML](http://gettaurus.org/docs/YAMLTutorial/?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium) format. Through them, Taurus takes the list of user interface actions, translates them into program code that uses Selenium and also executes the generated script.

In this article we will demonstrate how to build a simple YAML script, which describes actions performed by a user on a web page. Then, we will reuse this scenario for load testing.

## Step 1. Creating a Simple YAML Script

Let's create a simple script for one page in the Chrome browser in YAML. 

What we need to begin to work: 
 1. [Download and install Taurus](http://gettaurus.org/install/Installation/?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium)
 1. Download and install the latest version of Chrome 
 1. Download [ChromeDriver](https://sites.google.com/a/chromium.org/chromedriver/) 
 1. Include the ChromeDriver location in your PATH environment variable 
 1. Create a simple YAML script:

```yaml
execution:
- executor: selenium
  scenario: open_page

scenarios:
  open_page:
    browser: Chrome
    timeout: 10s
    think-time: 3s
    requests:
    - url: http://blazedemo.com
```

This script, which is a simple GET request, will be performed as a Selenium test in Chrome. It will load the first page of our demo site [http://blazedemo.com](http://blazedemo.com?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium).

`timeout` - global scenario timeout for connecting, receiving results, 30 seconds by default, notice we changed it to 10s.
`think-time` - global scenario delay between each request
`browser` - displays which browsers support our script implementation. It's possible to use it in Chrome, Firefox, Ie and Opera.

Now run the script with command-line:
```bash
bzt load_page.yml
```

The Taurus Console displays the execution of this one test:

![](sel-act1.png)

The results of the test are displayed in the console the by default. If you want to see the testing process in detail you can look it up in the _artifacts directory_. The artifacts directory is created in the same folder where the script was launched from. If you want your artifact directory to be created in another location, you can change the artifact directory route. Look [here](http://gettaurus.org/docs/ConfigSyntax/#Top-Level-Settings?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium) for more information about how this can be done.

The name of the folder is created according to the format `%Y-%m-%d\_%H-%M-%S.%f`. This folder stores some files that interest us:
 - `test\_requests.py` - the Selenium test generated in Python
 - `selenium.err`/`selenium.out` - the result of execution selenium tests and error message.
 - `webdriver.log` - the webdriver log
 - `bzt.log` - the Taurus log, very detailed, great source for troubleshooting the tool

`test\_requests.py` looks like this:
 
![](sel-act2.png)

## Step 2. Adding UI Actions for the Webpage

Now, let's add actions that can be performed on the webpage we are testing. 

All actions on the page operate with WebElements, which correspond to any visual object on the page as well as to invisible objects on the page. For example, links, images, text fields, etc. Each element is described by an HTML tag, even though sometimes elements do not originate from HTML but from Javascript generated pieces of the DOM tree. To perform any actions on the element, the element first needs to be to be located on the page. One element can be described using different locators types.

Let's take a look at the locators we are going to use and that support Selenium and Taurus: ID, Name, CSS, XPath.

But first, we need to put the site address before the request in the script. Here we are looking at blazedemo.com:

```yaml
...
default-address: http://blazedemo.com
requests:
- url: /purchase.php
```

To find the locators, open the HTML code of the page you are testing:

![](sel-act3.png)

The Header is inside the tag `<h2>`. This tag is cannot be found anywhere else in the source of the page we work with. So, we need to grab the element by this tag. Add the visible option to the selector in your script to validate the found element actually displayed on the page.

```yaml
...
actions:
- waitByCSS(h2): visible 
```

If we fill out the fields of the form on blazedemo.com, we get this HTML:

![](sel-act4.png)

The text field Name has the attribute ID (`id="inputName"`), so Use this locator in the script to find this Input. This ID is unique on this webpage. 

- `keysByID(inputName): Myname`

Click on the submit button on the blazedemo webpage:

![](sel-act5.png)

This button has attribute CLASS (`class="btn btn-primary"`) and this class is unique on this page. So, we can use the CSS selector and add it to the script.

- `clickByCSS(.btn.btn-primary)`

Go to the webpage itself page and validate the page contents. 

![](sel-act6.png)

Add an assertion to the script:
```yaml
...
assert:
- contains:
   - 'Thank you for your purchase today!'
```
Here is our complete script code:

```yaml
execution:
- executor: selenium
  scenario: open_page

scenarios:
  open_page:
    browser: Chrome
    timeout: 10s
    think-time: 3s
    default-address requests: http://blazedemo.com
    requests:
    - url: /purchase.php
      actions:
      - waitByCSS(h2): visible
      - keysByID(inputName): MyName
      - clickByCSS(.btn.btn-primary)
      assert:
      - contains:
        - 'Thank you for your purchase today!'
```

Now, we've written one test, which can be run once. 

### Supported Commands

Taurus supports the following commands:
 
`waitByID`, `waitByName`, `waitByCSS` and `waitByXPath` - wait till the element is  present on the page. Find element by ID, Name, CSS or XPath. 

`clickByID`, `clickByName`, `clickByCSS` and `clickByXPath` - allows to click on the object.

`keysByID`, `keysByName`, `keysByCSS` and `keysByXPath` - inserts values in the found element.


### Locators Reference

__Locating by ID. (ID selector)__

This is the simplest method.
```html
<td>
    <h1 id="my_ID_locator">ID Locator</h1>
</td>
```

Header tag has an ID attribute (`id="ID\_locator"`).

For example, wait until the element is displayed `waitByID(ID\_locator)`.

The biggest disadvantage of the method is that ids can be dynamically generated when the page is loaded. In this case we cannot use the element determination by its ID.

__Locator determination by the attribute Name__

`\<input name="inputName">`
This is an input element and it has attribute `name="inputName"`. 

For example, insert text the following in the input `keysByName(inputName): first\_name`

Locator determination by the attribute `name` is often used when working with input fields.

__Locating Elements by CSS Selector__

CSS is used for styling different elements of an HTML webpage, to separate the content of the page and its design. The .css files define these styles, set font size, width, height etc. There are certain patterns, which act as selectors, in the CSS to apply those styles to HTML elements of the page. Selenium uses the same principle to find items.

Consider a few basic selectors with CSS

__Using ID__

Find the field for input name with `id="inputName"`

`\<input id="inputName" placeholder="First Last" name="inputName" type="text">`

CSS selector - `#inputName`. Example: add text in the field `keysByCSS(#inputName): first\_name`

__Using CLASS__

Find div with input.
```html
<div class="controls">
    <input id="inputName" placeholder="First Last" name="inputName" type="text">
</div>
```
This CSS selector .controls will select all elements with class `controls`.

For example, wait until the element is displayed `waitByCSS(.controls)`.

Using attributes and their value
```html
 <input id="inputName" placeholder="First Last" name="inputName" type="text">
```

Find the element by `name="inputName"`. For example, `keysByCSS(\[name='inputName']): first\_name`

__Locating child elements__
```html
<div class="controls">
    <input id="inputName" placeholder="First Last" name="inputName" type="text">
</div>
```
Find child element INPUT with `id="inputName"` in the div with `class="controls"`. Describe as `div.controls>input#inputName`. For example, `keysByCSS(div.controls>input#inputName): first\_name`

This is a short description of the CSS selectors operation. The main advantage of working with CSS locators is the speed of their work. They work much faster than XPath.

__Locating by XPath__

XPath is the language used for locating nodes in an XML document. As HTML can be an implementation of XML (XHTML), Selenium can use this language to find elements for the web page. One of the main reasons for using XPath is when you don't have a suitable id or name attribute for the element you wish to locate. To locate the element we can use absolute XPath or use relative path elements that have attributes id, name etc. 

For INPUT  
```html
<div class="controls">
	<input placeholder="First Last" type="text">
</div> 
```

We can compose following XPath expressions: 
 - `//div/input`
 - `//div\[@class="controls"]/input`

Any of these expressions can be used to fetch the desired element, if these attributes are unique.

## Step 3: Measuring Multiple Stages in Your Script

Taurus can measure the different stages of your test as separate KPIs. So let's add a new part to the script, which loads a page with a large image. 
```yaml
scenarios:
  open_page:
    browser: Chrome
    timeout: 10s
    think-time: 3s
    default-address: http://blazedemo.com
    requests:
    - url: /purchase.php
      actions:
      - waitByCSS(h2): visible
      - keysByID(inputName): MyName
      - clickByCSS(.btn.btn-primary)
      assert:
      - contains:
        - 'Thank you for your purchase today!'
    - url: vacation.html
      actions:
      - waitByCSS(div.container img): visible
```
Run `bzt config.yml`

 The Taurus Console displays the execution and measurements of both stages:

![](sel-act7.png)

We can see the different KPIs for each stage.


## Step 4. Adding the Load Steps to the Test

Now we will add some load parameters to the script.

Concurrency: 5 concurrent users
Ramp-up time: 2 minutes
Time to hold the load: 5 minutes

Set:
```yaml
execution:
- executor: selenium
  scenario: open_page
  concurrency: 5
  ramp-up: 2m
  hold-for: 5m

scenarios:
  open_page:
    browser: Chrome
    timeout: 10s
    think-time: 3s
    default-address: http://blazedemo.com
    requests:
    - url: /purchase.php
      actions:
      - waitByCSS(h2): visible
      - keysByID(inputName): MyName
      - clickByCSS(.btn.btn-primary)
      assert:
      - contains:
        - 'Thank you for your purchase today!'
```

Run by typing `bzt -cloud  load_page.yml`

![](sel-act8.png)

Now we have a load script that is loaded by default in the local environment. The Concurrency Option will work in the [Blazemeter Cloud](https://a.blazemeter.com/app/sign-up?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium). To get instructions on how to adjust settings of execution in the cloud, please, see [here](http://gettaurus.org/docs/Cloud/?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium). [BlazeMeter](http://info.blazemeter.com/request-demo-4?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium) also enables you to view [rich reports](https://www.blazemeter.com/blog/understanding-your-reports-part-4-how-read-your-load-testing-reports-blazemeter?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium) that analyze KPIs and monitor system health.


Congratulations! You can now write and run webpage load scripts by using an expression of actions through your YAML file. Taurus can also be [integrated with Jenkins](http://gettaurus.org/kb/Jenkins/?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium), for continuous integration purposes.

To get a BlazeMeter demo, [click here](http://info.blazemeter.com/request-demo-4?utm_source=taurus&utm_medium=KB&utm_campaign=TaurusSelenium).
