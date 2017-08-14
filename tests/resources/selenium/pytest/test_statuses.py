import pytest


def test_passed():
    pass


def test_failed():
    assert 2 + 2 * 2 == 8


def test_broken():
    raise Exception("Ima broke")


@pytest.mark.skip(reason="skippy")
def test_skipped():
    raise Exception("If you see it - ima broke")
