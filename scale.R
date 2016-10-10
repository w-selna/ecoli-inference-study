count.list = list()
list_space = 1
links = 0
for (b in final)
{
  for (r in 1:nrow(link_40.list))
  {
    if (b == link_40.list[r,1] || b == link_40.list[r,2] )
    {
      links  = links+1
    }
  }
  count.list[list_space] = links
  list_space = list_space +1
  links = 0
}
  