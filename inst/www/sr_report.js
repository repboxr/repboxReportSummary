document.addEventListener("DOMContentLoaded", function() {
  var rows = document.querySelectorAll('.sr-issue-row');

  rows.forEach(function(row) {
    row.addEventListener('click', function() {
      var targetId = row.getAttribute('data-target');
      var detailRow = document.getElementById(targetId);

      if (!detailRow) return;

      var isExpanded = detailRow.style.display !== 'none';

      if (isExpanded) {
        detailRow.style.display = 'none';
        row.classList.remove('active');
      } else {
        detailRow.style.display = 'table-row';
        row.classList.add('active');
      }
    });
  });
});
