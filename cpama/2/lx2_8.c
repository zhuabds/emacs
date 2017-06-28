#include <stdio.h>

main()
{
  float dollar_amount = 0, tax_dollar_amount = 0;

  printf("Enter a dollar amount:");
  scanf("%f", &dollar_amount);

  tax_dollar_amount = dollar_amount * (1 + 0.05);
  printf("With tax added: %.2f\n", tax_dollar_amount);

  return 0;
}
