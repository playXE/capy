function loop(n) {
    if (n == 0) {
        return;
    } else {
        return loop(n - 1);
    }
}

loop(1000);