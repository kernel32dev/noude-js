
function main() {
    for (let n = 2; n < 100; n++) {
        if (is_prime(n)) {
            console.log(n);
        }
    }
}

function is_prime(n) {
    for (let k = 2; k < n; k++) {
        if (n % k === 0) {
            return false;
        }
    }
    return true;
}

main();

