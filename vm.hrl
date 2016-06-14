%% A Vending Machine has a list of products
%% to offer and some money for change.
-record(vm, {products = [] :: [tuple()], money :: float()}).
