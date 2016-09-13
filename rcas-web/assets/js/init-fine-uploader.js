var uploader = new qq.FineUploader({
  element: document.getElementById('fine-uploader'),
  multiple: false,
  request: {
    endpoint: '/uploads'
  },
  failedUploadTextDisplay: {
    mode: 'custom',
    responseProperty: 'error'
  },
  callbacks: {
    onComplete: function(id, name, responseJSON, xhr) {
      if (responseJSON['success']) {
        window.location = "/result/"+responseJSON['result'];
      }
    }
  }
});
