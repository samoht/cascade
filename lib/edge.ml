let selectors sel =
  match Selector.as_list sel with Some xs -> xs | None -> [ sel ]
