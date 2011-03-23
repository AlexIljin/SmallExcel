@echo off
rem Checkout repo_commit out of a local repository at repo_path, then move the
rem necessary parts to the folders we need them at.
setlocal
set repo_path=s:/git/Library/.git
set repo_commit=dd5308950854c1aceb053dec3418151020891760
set random_id=%RANDOM%
set checkout_path=%~dp0Checkout-tmp%random_id%
set GIT_INDEX_FILE=%~dp0Checkout-index%random_id%

rem Make checkout in a temp checkout_path folder
md %checkout_path%
cd %checkout_path%
call git --git-dir=%repo_path% read-tree %repo_commit%
call git --git-dir=%repo_path% checkout-index -a

rem Get the necessary parts of the checkout
move %checkout_path%\A3 ..
move %checkout_path%\A3Lib ..
move %checkout_path%\A3Edit ..

rem Delete the temporary files
cd ..
del %GIT_INDEX_FILE%
rd /s /q %checkout_path%
