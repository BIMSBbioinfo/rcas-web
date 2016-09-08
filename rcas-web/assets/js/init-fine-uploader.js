var uploader = new qq.FineUploader({
  element: document.getElementById('fine-uploader'),
  multiple: false,
  request: {
    endpoint: '/uploads'
  }
});
