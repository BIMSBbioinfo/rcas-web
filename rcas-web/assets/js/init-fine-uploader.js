var uploader = new qq.FineUploader({
  debug: true,
  element: document.getElementById('fine-uploader'),
  multiple: false,
  request: {
    endpoint: '/uploads'
  }
});
