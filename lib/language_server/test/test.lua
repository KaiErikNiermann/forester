vim.filetype.add({ extension = { tree = "forester" } })

vim.cmd("edit trees/index.tree")

local client_id = vim.lsp.start({
	name = "forester-lsp",
	cmd = { "forester", "lsp" },
	root_dir = vim.fs.root(0, { "forest.toml" }),
})

vim.print(string.format("client_id: %d", client_id))
vim.print(string.format("filetype: %s", vim.bo.filetype))

local bufnr = vim.api.nvim_get_current_buf()

vim.print(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false))

vim.print(vim.lsp.get_clients())

local symbols = vim.lsp.buf.document_symbol()
vim.print(vim.inspect(symbols))
