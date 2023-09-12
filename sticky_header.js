window.addEventListener('scroll', function () {
    var header = document.querySelector('.sticky-header');

    if (window.pageYOffset > 0) {
        header.classList.add('sticky');
    } else {
        header.classList.remove('sticky');
    }
});