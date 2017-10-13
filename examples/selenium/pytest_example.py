def test_arithmetic_gotcha():
    assert 2 + 2 * 2 == 6

def test_gotcha():
    assert "Hello" + ", World!" == "Hello, World!"
