x = [2.4, 5.9, 8.4, 12.9, 16.4];
y = [5.76, 34.81, 88.36, 166.41, 268.96]

sum = 0;
for i = 1:length(x)
    p = 1;
    for j = 1:length(x)
        if j~= i
            c = poly(x(j))/(x(i) - x(j));
            p = conv(p, c);
        end
    end
    term = p*y(i);
    sum = sum + term;
end
disp(sum);

xx = linspace(min(sum), max(sum), 100);
yy = sum(1)*xx.^3 + sum(2)*xx.^2 + sum(3)*xx + sum(4);
plot(x, y, '-s')
hold on 
plot(xx, yy, '-r')
%plot(sum, '-o')
xlabel('x'), ylabel('y'), title('Lagrrange poly test')
legend('Data collection', 'lagrange poly fit', 'location', 'northeast')