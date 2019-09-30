import java.math.BigInteger;

public class Fibonacci {

    public static BigInteger[][] dot(BigInteger[][] A, BigInteger[][] B) {
        BigInteger[][] R = new BigInteger[2][2];
        for (int i = 0; i < 2; i++) {
            for (int j = 0; j < 2; j++) {
                R[i][j] = BigInteger.valueOf(0);
                for (int k = 0; k < 2; k++) {
                    R[i][j] = R[i][j].add(A[i][k].multiply(B[k][j]));
                }
            }
        }
        return R;
    }

    public static BigInteger pow(BigInteger[][] A, int n, int flag) {
        BigInteger[][] result = new BigInteger[2][2];
        result[0][0] = result[1][1] = BigInteger.valueOf(1);
        result[0][1] = result[1][0] = BigInteger.valueOf(0);
        while (n > 0) {
            if (n % 2 == 1) {
                result = dot(result, A);
            }
            n /= 2;
            A = dot(A, A);
        }
        if (flag == 1) return result[1][0];
        else return result[0][0];
    }

    public static BigInteger fib(BigInteger n) {
        int N = n.intValue();
        BigInteger[][] a = new BigInteger[2][2];
        BigInteger[][] b = new BigInteger[2][2];

        a[0][0] = BigInteger.valueOf(1);
        a[0][1] = BigInteger.valueOf(1);
        a[1][0] = BigInteger.valueOf(1);
        a[1][1] = BigInteger.valueOf(0);

        b[0][0] = BigInteger.valueOf(0);
        b[0][1] = BigInteger.valueOf(1);
        b[1][0] = BigInteger.valueOf(1);
        b[1][1] = BigInteger.valueOf(-1);

        if (N > 0) return pow(a, N, 1);
        else return pow(b, 1-N, -1);

    }

}