char	*my_strcpy(char *dest, char *src)
{
  char	*adrr;

  for (adrr = dest; *src; *dest++ = *src++);
  *dest++ = '\0';
  return (adrr);
}
