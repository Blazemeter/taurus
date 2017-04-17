import re


def headers_as_text(headers_dict):
    return "\n".join("%s: %s" % (key, value) for key, value in headers_dict.items())


def shorten(string, upto, end_with="..."):
    return string[:upto - len(end_with)] + end_with if len(string) > upto else string


def assert_regexp(regex, text, match=False, msg=None):
    if match:
        if re.match(regex, text) is None:
            msg = msg or "Regex %r didn't match expected value: %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)
    else:
        if not re.findall(regex, text):
            msg = msg or "Regex %r didn't find anything in text %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)


def assert_not_regexp(regex, text, match=False, msg=None):
    if match:
        if re.match(regex, text) is not None:
            msg = msg or "Regex %r unexpectedly matched expected value: %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)
    else:
        if re.findall(regex, text):
            msg = msg or "Regex %r unexpectedly found something in text %r" % (regex, shorten(text, 100))
            raise AssertionError(msg)
