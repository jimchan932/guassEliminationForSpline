#include <iostream>
#include <vector>
//#include "tridiag.h"
#define MAXSIZE 100
using namespace std;
//#define gamma 234
// solve for x



void tridiag(double* a, double* b, double* c,  double* r,  double* u , int n)
{

	int j;
	double bet;
	
	double *gam = new double[n];
	u[0] = r[0] / (bet = b[0]);
	for(j = 1; j < n; j++) // 0 ... n-1
	{
		gam[j] = c[j-1] / bet;
		bet = b[j] - a[j]*gam[j];
		if(bet == 0.0)
		{
			std::cout <<"error: 0 for local variable"; return;
		}
		u[j] = (r[j] -a[j]*u[j-1])/bet;
	}
	for(j=(n-2); j>= 0; j--)
	{
		u[j] -= gam[j+1] * u[j+1];
	}
	delete []gam;
}

void cyclic(double *a, double *b, double *c, double alpha, double beta,
			double*r, double*x, int n)
{
	int i;
	double fact; double gamma;
	gamma = -444.0;
	double* bb = new double[n];
	double *u = new double[n];
	double *z = new double[n];
	bb[0] = b[0] - gamma;
	bb[n-1] = b[n-1] - alpha*beta/gamma;
	for(i = 1; i < n-1; i++)
	{
		bb[i] =b[i];
	}
	tridiag(a, bb, c, r, x, n);
	u[0] = gamma;
	u[n-1] = alpha;
	for(i = 1; i < n-1; i++) u[i] =0.0;
	tridiag(a, bb, c,u,z, n);
	fact = (x[0] + beta*x[n-1]/gamma)/
		(1.0 + z[0] + beta*z[n-1]/gamma);
	for(int i = 0; i < n; i++) x[i] -= fact*z[i]; // solve solution vector x

}

int main()
{
	int n;
    double x[MAXSIZE];
    double y[MAXSIZE];
	double lambda[MAXSIZE];
	double u[MAXSIZE];
	double diag[MAXSIZE];
    double differenceX[MAXSIZE];
	double f[MAXSIZE];
	double solution[MAXSIZE];
	std::cin >> n;	

	x[0] = 0;
	std::cout << "Input x: ";
	/*
	for(int i = 0; i < n+1; i++)
	{   // x0 to x(n)

		double tempX;
		std::cin >> tempX;
		x[i] = tempX;
		}*/

	for(int i = 0; i < n+1; i++)
	{   // x0 to x(n)

		double tempX;
		std::cin >> tempX;	
		x[i] = tempX;
	}	
 	for(int i = 0; i < n+1; i++)
	{   // y0 to y(n)
		double tempY;
		std::cin >> tempY;
		y[i] = tempY;
	}
	for(int i = 0; i < n; i++)
	{
		double tempDiff = x[i+1] - x[i]; // h0 to h(n-1)
		differenceX[i] = tempDiff;       // 0 to n-1
	}
	
	for(int i = 0; i < n-1; i++)
	{
		lambda[i] = differenceX[i+1] / (differenceX[i] + differenceX[i+1]);
	}
	lambda[n-1] = 0;
	u[0] = 0;
	for(int i = 1; i < n; i++)
	{
		u[i] = differenceX[i-1] / (differenceX[i] + differenceX[i-1]);
	}
	double lambdaN = differenceX[0] / (differenceX[n-1] + differenceX[0]);
    double Un = 1- lambda[n-1];
	
	
	for(int i = 0; i < n; i++)
	{
		diag[i] = 2;
	}
	double diff0 = (y[1] - y[0]) /(differenceX[0]);	
	for(int i = 0; i < n-1; i++)
	{		
		double diffi = (y[i+2] - y[i+1]) / (differenceX[i+1]);		
		double diffiPlus1 = (y[i+1] - y[i]) / (differenceX[i]);
		f[i] =  6* (diffiPlus1 - diffi)/ (differenceX[i] + differenceX[i+1]);
	}
	double diffn = (y[n] - y[n-1]) /(differenceX[n-1]);		
	f[n-1] =  6* (diff0  - diffn)/ (differenceX[0] + differenceX[n-1]);	


	cyclic(lambda, diag, u, lambdaN, Un,
		   f, solution, n);



	for(int i = 0; i < n; i++)
	{
		std::cout << solution[i]<<" ";
	}
		   
	return 0;
}

