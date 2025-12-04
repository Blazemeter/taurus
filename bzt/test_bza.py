import unittest
from unittest.mock import MagicMock, patch
from bzt.bza import Test

class TestUploadFilesCount(unittest.TestCase):
    @patch('bzt.bza.MultiPartForm')
    def test_uploads_all_files(self, MockMultiPartForm):
        mock_form = MagicMock()
        MockMultiPartForm.return_value = mock_form
        mock_form.get_content_type.return_value = 'multipart/form-data'
        mock_form.form_as_bytes.return_value = b'data'

        test_obj = Test()
        test_obj['id'] = '123'
        test_obj.address = 'http://fake'
        test_obj.log = MagicMock()
        test_obj._request = MagicMock()

        taurus_config = 'config'
        resource_files = [f'file_{i}' for i in range(60)]
        test_obj.upload_files(taurus_config, resource_files)

        self.assertEqual(mock_form.add_file.call_count, 60)
        expected_calls = [((f'files[{i}]', f'file_{i}'),) for i in range(50)]
        expected_calls += [((f'files[{i}]', f'file_{i+50}'),) for i in range(10)]
        mock_form.add_file.assert_has_calls(expected_calls, any_order=False)
        self.assertEqual(test_obj._request.call_count, 2)

if __name__ == '__main__':
    unittest.main()