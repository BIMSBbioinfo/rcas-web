// @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL-v3.0-or-later
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
        window.location = "/result/"+responseJSON['result'] +
          // pass GALAXY_URL along (if exists)
          window.location.search;
      }
    }
  }
});
// @license-end
